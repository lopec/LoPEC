%%%-------------------------------------------------------------------
%%% @author Burbas, Vasilij Savin <>
%%% @doc
%%%
%%% Contains the unit tests for the job listener. 
%%%
%%% @end
%%% Created : 14 Oct 2009 by Vasilij <>
%%%-------------------------------------------------------------------
-module(listener_tests).
-include("../include/db.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_test_case() ->    
    db:create_tables().

end_per_test_case() ->
    db:delete_tables().

init_test() ->
    application:start(chronicler),
    application:start(ecg),
    db:start(),
    dispatcher:start_link(),
    listener:start_link().

%% Just tests general DB behaviour, so it does not blow up.
%% db_test() ->
%%     init_per_test_case(),
%%     JobId = dispatcher:add_job({raytracer, 0}),
%%     TaskSpec = {JobId, split, "", 0},
%%     TaskId =  dispatcher:create_task(TaskSpec),
%%     FreeTask = db:get_task(node()),
%%     JobType = (db:get_job_info(FreeTask#task.job_id))#job.job_type,
%%     end_per_test_case().

job_creation_test() ->
    init_per_test_case(),
    JobId = listener:new_job(raytracer, 
                             "/home/chabbrik/Desktop/ClusterBusters/input.file"),
%%     chronicler:info("JobID in test:" ++ JobId),
    Job = db:get_job_info(JobId),
    chronicler:info(Job),
    ?assertEqual(JobId, Job#job.job_id),
    ?assertEqual(raytracer, Job#job.job_type),

    {ok, Root} = 
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
%%     chronicler:info(Root),
    JobIdString = lists:flatten(io_lib:format("~p", [JobId])),
    ProgramFile = Root ++ "tmp/" ++ JobIdString ++ "/input/data.dat",
       chronicler:info(ProgramFile),
    ?assertEqual(ok, file:rename(ProgramFile, ProgramFile)),
    TaskList = db:list_tasks(JobId),
    ?assertMatch([A], TaskList),
    Task = db:get_task_info(hd(TaskList)),
    ?assertEqual(split, Task#task.task_type),
    ?assertEqual(JobId, Task#task.job_id),

%%     db:get_task(NodeId)
%%     TaskSpec = {JobId, split, "", 0},
%%     TaskId =  dispatcher:create_task(TaskSpec),
%%     dispatcher:get_task(node(), self()),
%%     receive
%%         {task_response, Task} ->
%%             ?assert(Task#task_tmp.task_id =:= TaskId),
%%             ?assertEqual(split, Task#task_tmp.task_type),
%%             ?assertEqual(JobId, Task#task_tmp.job_id);
%%         Msg ->
%%             chronicler:error(io_lib:format("Wrong message received: ~p", [Msg]))            
%%     end,
    end_per_test_case().



end_test() ->
    db:stop(),
    dispatcher:terminate(finished, []).