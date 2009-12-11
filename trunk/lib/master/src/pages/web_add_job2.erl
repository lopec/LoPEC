-module(web_add_job2).
-include_lib("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    case common_web:have_role([role_user, role_admin]) of
        true -> [];
        _ -> wf:redirect("/web/index")
    end,
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

body() ->
    %% Setting up Program Types
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    ProgramFiles = lists:filter(
        fun(X) -> filelib:is_dir(X) end, filelib:wildcard(Path ++ "/programs/*")),
    ProgramTypes = lists:map(fun(T) -> #option {text=T,value=T} end, ProgramFiles),

    %% Flash message
    Body = [#panel { body=[
        #h2{text="Add job"},
        #p{},
        #label { text="Program type:" },
        #dropdown { id=programType, options=
            ProgramTypes
        },
        #p{},
        #label { text="User: " },
        #span { text=wf:user() },
        #p{},
        #label { text="Priority:"},
        #textbox { id=priorityTextBox, next=continueButton },
        #p{},
        #label { text="Programfile:" },
        #upload { tag=programupload, show_button=true },
        #p{},
        #button { id=continueButton, text="Next", postback=add_job }
    ]}],

    %% Validators
    wf:wire(continueButton, priorityTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_integer { text="Must be an integer."}
    ]}),
    
    wf:render(Body).
    

%%%%%%%%%%%%%%%%%%%%%%%%
%% When a job is added %
%%%%%%%%%%%%%%%%%%%%%%%%
event(add_job) ->
    User = wf:user(),
    [ProgramTypePath] = wf:q(programType),
    ProgramType = hd(lists:reverse(string:tokens(ProgramTypePath, "/"))),
    wf:flash(wf:f(ProgramType)),
    [Priority] = wf:q(priorityTextBox),
    InputData = wf:state(inputdata),
    case InputData of 
        "" ->
            wf:flash(wf:f("You must give an inputfile"));
        _ ->
            ProblemType = "mapreduce",
            case listener:add_job(list_to_atom(ProgramType), list_to_atom(ProblemType), User, list_to_integer(Priority), InputData) of
                {ok, JobId} -> 
                    wf:clear_state(),
                    wf:redirect("/web/add/job");
                {error, Reason} -> 
                    wf:flash(wf:f(InputData)),
                    wf:flash(wf:f("Could not add job. Reason: ~w", [Reason]))
            end
        end;
event(_) -> ok.



%%%%%%%%%%%%%%%%%%%%%%
%% Upload file event %
%%%%%%%%%%%%%%%%%%%%%%
upload_event(_Tag, undefined, _) ->
  wf:flash("Please select a file."),
  ok;
upload_event(_Tag, FileName, LocalFileData) ->
    FileSize = filelib:file_size(LocalFileData),
    wf:state(inputdata, LocalFileData),
    wf:flash(wf:f("Uploaded file: ~s (~p bytes)", [FileName, FileSize])),
    {ok, LocalFileData}.

