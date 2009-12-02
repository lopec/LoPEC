-module (web_add_job).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

%% Todo:
%% - Fix so the uploaded file is used in the cluster
%% - Fix validation of the form
%% - A redirect to a new page when job is started
%% - Fix ProblemType and ProgramType input

main() ->
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

%% Help function to output jobs
get_jobs([]) ->
    [];
get_jobs([H|T]) ->
    IdString = lists:flatten(io_lib:format("~p", [H])),
    [
        #tablerow { cells=[
            #tablecell { body=[#link { body = [#image{ image = "/images/delete.gif", alt="Cancel job" }], postback={cancel, H}},
            #link { body = [#image{ image = "/images/pause.png", alt="Pause job" }], postback={pause, H}},
            #link { body = [#image{ image = "/images/resume.gif", alt="Resume job" }], postback={resume, H}},
            #link { body = [#image{ image = "/images/stop.jpg", alt="Stop job" }], postback={stop, H}}]},
            #tablecell { text=IdString },
            #tablecell { text="Running" }
        ]} 
    |[get_jobs(T)]].

get_job_table() ->
    #table { id=jobTable, 
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
    ProblemType = "mapreduce",
    ProgramType = "raytracer2",
    case listener:add_job(list_to_atom(ProgramType), list_to_atom(ProblemType), 
        User, list_to_integer(Priority), "/storage/test/lol.txt") of
        {ok, JobId} -> 
            wf:flash(wf:f("Added a new job with id: ~w", [JobId])),
            wf:update(jobTable, get_job_table());
        {error, Reason} -> 
            wf:flash(wf:f("Could not add job. Reason: ~w", [Reason]))
    end,
    ok;
%%%%%%%%%%%%%%%%%%
%% Cancels a job %
%%%%%%%%%%%%%%%%%%
event({cancel, JobId}) ->
    wf:wire(#confirm { text = "Are you sure that you want to cancel this job?", postback={confirm_cancel, JobId}});
event({confirm_cancel, JobId}) ->
    listener:cancel_job(JobId),
    wf:flash(wf:f("Removed job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());

event(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%
%% Upload file event %
%%%%%%%%%%%%%%%%%%%%%%
upload_event(_Tag, undefined, _) ->
  wf:flash("Please select a file."),
  ok;

upload_event(_Tag, FileName, LocalFileData) ->
    FileSize = filelib:file_size(LocalFileData),
    case filename:extension(FileName) of
        _ -> wf:flash(wf:f("Uploaded file: ~s (~p bytes)", [FileName, FileSize]))
    end,
    {ok, LocalFileData}.
