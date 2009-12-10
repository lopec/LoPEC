-module (web_add_job).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

%% Todo:
%% - Fix so the uploaded file is used in the cluster
%% - Fix ProgramName as input
%% - When the "Next" button is clicked, the flash-message should disappear

main() ->
    common_web:have_role([role_user, role_admin]),
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

% Creates the menu.
menu() ->
    common_web:menu().

% Creates the submenu.        
submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).

%% Help function to output jobs
get_jobs([]) ->
    [];
get_jobs([H|T]) ->
    IdString = lists:flatten(io_lib:format("~p", [H])),
    case db:job_status(H) of
        {ok, active} -> 
            Status = "Running",
            Buttons = [#link { body = [#image{ class="controlimage", image = "/images/delete.gif", alt="Cancel job" }], postback={cancel, H}},
                       #link { body = [#image{ class="controlimage", image = "/images/stop.jpg", alt="Stop job"}], postback={stop, H}},
                       #link { body = [#image{ class="controlimage", image = "/images/pause.png", alt="Pause job" }], postback={pause, H}}];
        {ok, paused} -> 
            Status = "Paused",
            Buttons = [#link { body = [#image{ class="controlimage", image = "/images/delete.gif", alt="Cancel job" }], postback={cancel, H}},
                       #link { body = [#image{ class="controlimage", image = "/images/stop.jpg", alt="Stop job"}], postback={stop, H}},
                       #link { body = [#image{ class="controlimage", image = "/images/resume.gif", alt="Resume job"}], postback={resume, H}}];
        {ok, stopped} -> 
            Status = "Stopped",
            Buttons = [#link { body = [#image{ class="controlimage", image = "/images/delete.gif", alt="Cancel job" }], postback={cancel, H}},
                       #link { body = [#image{ class="controlimage", image = "/images/resume.gif", alt="Resume job"}], postback={resume, H}}];
        {ok, no_tasks} -> 
            Status = "Finished",
            Buttons = [];
        _ ->
            Status = "Error.",
            Buttons = []
    end,
    [
        #tablerow { cells=[
            #tablecell { body=Buttons },
            #tablecell { body=#link { text=IdString, url="/web/view/job/" ++ IdString } },
            #tablecell { text=Status }
        ]} 
    |[get_jobs(T)]].

get_job_table() ->
    #table { id=jobTable, class="jobtable", 
        rows = [
            #tablerow { cells=[
                #tableheader { text="Controls  " },
                #tableheader { text="JobId" },
                #tableheader { text="Status" }
                ]}
                |get_jobs(db:get_user_jobs(wf:user()))
        ]
    }.

%% Body function
body() -> 
   %% Status of all jobs 
    case wf:get_path_info() of 
        "" -> 
            Body = [
                #button { text="Add new job", postback=add_job_box },
                #p {},
                get_job_table()
            ];
        _JobString -> 
            Body = []
    end,
    wf:comet(fun() -> update_jobtable() end),
    wf:render(Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENTS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%
%% Add Job-Box event %
%%%%%%%%%%%%%%%%%%%%%%
event(add_job_box) ->
    %% Setting up Program Types
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    ProgramFiles = lists:filter(
        fun(X) -> filelib:is_dir(X) end, filelib:wildcard(Path ++ "/programs/*")),
    ProgramTypes = lists:map(fun(T) -> #option {text=T,value=T} end, ProgramFiles),

    %% Flash message
    wf:flash([
        #h2{text="Add job"},
        #p{},
        #label { id=programType, text="Program type:" },
        #dropdown { options=
            ProgramTypes
        },
        #p{},
        #label { text="Priority:"},
        #textbox { id=priorityTextBox, next=continueButton },
        #p{},
        #label { text="Programfile:" },
        #upload { tag=programupload, show_button=true },
        #p{},  
        #button { id=continueButton, text="Next", postback=add_job }
    ]),

    %% Validators
    wf:wire(continueButton, priorityTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_integer { text="Must be an integer."}
    ]});
%%%%%%%%%%%%%%%%%%%%%%%%
%% When a job is added %
%%%%%%%%%%%%%%%%%%%%%%%%
event(add_job) ->
    User = wf:user(),
    [Priority] = wf:q(priorityTextBox),
    InputData = wf:state(inputdata),
    case InputData of 
        "" ->
            wf:flash(wf:f("You must give an inputfile"));
        _ ->
            ProblemType = "mapreduce",
            ProgramType = "raytracer2",
            case listener:add_job(list_to_atom(ProgramType), list_to_atom(ProblemType), User, list_to_integer(Priority), InputData) of
                {ok, JobId} -> 
                    wf:flash(wf:f("Added a new job with id: ~w", [JobId])),
                    wf:update(jobTable, get_job_table());
                {error, Reason} -> 
                    wf:flash(wf:f("Could not add job. Reason: ~w", [Reason]))
            end
        end,
    wf:clear_state(),
    ok;
%%%%%%%%%%%%%%%%%%%
%% Controls a job %
%%%%%%%%%%%%%%%%%%%
event({cancel, JobId}) ->
    wf:wire(#confirm { text = "Are you sure that you want to CANCEL this job?", postback={confirm_cancel, JobId}});
event({stop, JobId}) ->
    wf:wire(#confirm { text = "Are you sure that you want to STOP this job?", postback={confirm_stop, JobId}});
event({resume, JobId}) ->
    wf:wire(#confirm { text = "Are you sure that you want to RESUME this job?", postback={confirm_resume, JobId}});
event({pause, JobId}) ->
    wf:wire(#confirm { text = "Are you sure that you want to PAUSE this job?", postback={confirm_pause, JobId}});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% When user pressed "yes" in the msgbox %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event({confirm_cancel, JobId}) ->
    listener:cancel_job(JobId),
    wf:flash(wf:f("Removed job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());
event({confirm_pause, JobId}) ->
    listener:pause_job(JobId),
    wf:flash(wf:f("Paused job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());
event({confirm_stop, JobId}) ->
    listener:stop_job(JobId),
    wf:flash(wf:f("Stopped job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());
event({confirm_resume, JobId}) ->
    listener:resume_job(JobId),
    wf:flash(wf:f("Resumed job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());
event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
%%%%%%%%%%%%
% Basecase %
%%%%%%%%%%%%
event(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%
%% Upload file event %
%%%%%%%%%%%%%%%%%%%%%%
upload_event(_Tag, undefined, _) ->
  wf:flash("Please select a file."),
  ok;

upload_event(_Tag, FileName, LocalFileData) ->
    FileSize = filelib:file_size(LocalFileData),
    wf:state(inputdata, "/storage/test/rayscene2048"),
    wf:flash(wf:f("FIX THIS WHEN I/O MODULE IS WORKING! Uploaded file: ~s (~p bytes)", [FileName, FileSize])),
    {ok, LocalFileData}.


%%%%%%%%%%%%%%%%%
% COMET PROCESS %
%%%%%%%%%%%%%%%%%
update_jobtable() ->
    timer:sleep(5000),
    wf:update(jobTable, get_job_table()),
    wf:comet_flush(),
    update_jobtable().
