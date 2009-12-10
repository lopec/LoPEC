-module (web_list_jobs).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).


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

get_jobs([]) ->
    [];
get_jobs([H|T]) ->
    IdString = lists:flatten(io_lib:format("~p", [H])),
    [
        #tablerow { cells=[
            #tablecell { body=[#link { body = [#image{ image = "/images/delete.gif" }], postback={delete, H}}]},
            #tablecell { text=IdString }
        ]} 
    |[get_jobs(T)]].

get_job_table() ->
    #table { id=jobTable, 
        rows = [
            #tablerow { cells=[
                #tableheader { text="Delete     " },
                #tableheader { text="JobId" }
            ]}|get_jobs(db:list_active_jobs())
        ]
    }.

body() ->
    case wf:get_path_info() of 
        "" -> 
            Body = [
                get_job_table()
            ];
        JobString -> 
            JobId = list_to_integer(JobString),
            Stats = statistician:get_job_stats(JobId, string),
            case Stats of 
                {error, Reason} -> Body = [#label { text=Reason }];
                _ -> Body = [#label{text=Stats}]
            end
    end,
 
    wf:render(Body).

event({delete, JobId}) ->
    wf:wire(#confirm { text = "Are you sure?", postback={confirm_delete, JobId}});
event({confirm_delete, JobId}) ->
    listener:cancel_job(JobId),
    wf:flash(wf:f("Removed job with Id: ~w", [JobId])),
    wf:update(jobTable, get_job_table());
event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
event(_) -> ok.
