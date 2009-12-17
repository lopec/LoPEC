-module(web_view_job).
-include_lib ("nitrogen/include/wf.inc").
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

menu() ->
    common_web:menu().

submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).



body() ->
    case wf:get_path_info() of
        %% The adress does not include a JobId
        "" ->
            Body = [
                #panel { class="errorbox", body=[
                    #label { class="title", text="Error" },
                    #label { class="message", text="No JobId given!" }
                ]}
            ];
        %% The JobId is given from the adress
        JobId ->
            wf:state(jobid, string:to_integer(JobId)),
            Body = [
                #h2 { text="Detailed information" },
                #panel { id=finished },
                #table { id=progressTable, class="progressTable" },
                #p {},
                #panel { id=statPanel }
            ]
    end,
    wf:comet(fun() -> get_progress() end),
    wf:render(Body).



event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
event(_) -> ok.


%% LOL @ background-size is parsed out :(((((((
get_table_cell(Width) ->
    Background = io_lib:format("~.16B", [(Width+155)]), 
    WidthString = lists:concat([Width]),
    #tablecell { class="progressbar", style="background-color: #00" ++ Background ++ "00;", 
        text=WidthString ++ "%" }.

%%%%%%%%%%%%%%%%%
% COMET PROCESS %
%%%%%%%%%%%%%%%%%
get_progress() ->
    % We get the jobid from state
    {JobId,_} = wf:state(jobid),
    case db:get_job(JobId) of
    {error, _Reason} ->
        Stats = #panel{ id=statPanel, body=[
            #label { text="Could not find any stats. Try again later."}
        ] },
        FinalizeDone = 0,
        wf:update(statPanel, Stats);
    _ ->     
        % We get the values from database
        {ok, {free, SFree}, {assigned, SAssigned}, {done, SDone}} = 
            db:task_info_from_job(split, JobId),
        {ok, {free, MFree}, {assigned, MAssigned}, {done, MDone}} =
            db:task_info_from_job(map, JobId),
        {ok, {free, RFree}, {assigned, RAssigned}, {done, RDone}} = 
            db:task_info_from_job(reduce, JobId),
        {ok, {free, FFree}, {assigned, FAssigned}, {done, FDone}} =
            db:task_info_from_job(finalize, JobId),
        case statistician:get_job_stats(JobId, raw) of
        [Power,Time,Upload,Download,Numtasks,Restarts] ->
            Stats = #panel{ id=statPanel, body=[
            #label { text=lists:concat(["Power spent: ", 
                erlang:trunc(((Power))/3600), "Wh"]) },
            #label { text=lists:concat(["Time spent: ", erlang:trunc(Time),
                                        " s"]) },
            #label { text=lists:concat(["Uploaded: ", 
                erlang:trunc(((Upload/1024)/1024)), "MB"]) },
            #label { text=lists:concat(["Downloaded: ", 
                erlang:trunc(((Download/1024)/1024)), "MB"]) },
            #label { text=lists:concat(["Number of tasks: ", Numtasks]) },
            #label { text=lists:concat(["Number of restarts: ", Restarts]) }
        ] };
        {error, _} ->
            Stats = #panel{ id=statPanel, body=[
                #label { text="Could not find stats for job" }
            ] }
        end,
        
        % Sum the values
        SplitSum = SFree+SAssigned+SDone,
        MapSum = MFree+MAssigned+MDone,
        ReduceSum = RFree+RAssigned+RDone,
        FinalizeSum = FFree+FAssigned+FDone,
        case SplitSum of
            0 -> SplitDone = 0;
            _ when SDone /= 0 -> 
                SplitDone = erlang:trunc(100*((100/SplitSum)/(100/SDone)));
            _ -> SplitDone = 0
        end,
        case MapSum of
            0 -> MapDone = 0;
            _ when MDone /= 0 -> 
                MapDone = erlang:trunc(100*((100/MapSum)/(100/MDone)));
            _ -> MapDone = 0
        end,
        case ReduceSum of
            0 -> ReduceDone = 0;
            _ when RDone /= 0 ->
                ReduceDone = erlang:trunc(100*((100/ReduceSum)/(100/RDone)));
            _ -> ReduceDone = 0
        end,
        case FinalizeSum of
            0 -> 
                FinalizeDone = 0;
            _ when FDone /= 0 -> 
                FinalizeDone = erlang:trunc(100*((100/FinalizeSum)/(100/FDone)));
            _ -> 
                FinalizeDone = 0
        end,

        Table = [
            #table { class="progressTable", rows=[
                #tablerow { id=progressTable, cells=[
                    #tableheader { text="Split" },
                    #tableheader { text="Map" },
                    #tableheader { text="Reduce" },
                    #tableheader { text="Finalize" }
                ]},
                #tablerow { cells=[
                    get_table_cell(SplitDone),
                    get_table_cell(MapDone),
                    get_table_cell(ReduceDone),
                    get_table_cell(FinalizeDone)
                ]}
            ]}
        ],
        case FinalizeDone of
            100 -> 
                GetResult = #link { text="Get result", url="#" };
            _ -> GetResult = ""
        end,
        wf:update(finished, GetResult),
        wf:update(progressTable, Table),
        wf:update(statPanel, Stats)
    end,
    wf:comet_flush(),
    timer:sleep(1000),
    case FinalizeDone of
        100 -> ok;
        _ -> get_progress()
    end.


