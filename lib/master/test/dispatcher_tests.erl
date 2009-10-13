%%%-------------------------------------------------------------------
%%% @author Axel, Vasilij Savin <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Contains the unit tests for the dispatcher. 
%%%
%%% @end
%%% Created : 1 Oct 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher_tests).
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
    dispatcher:start_link().

%% Just tests general DB behaviour, so it does not blow up.
db_test() ->
    init_per_test_case(),
    JobId = dispatcher:add_job({raytracer, 0}),
    TaskSpec = {JobId, split, "", 0},
    TaskId =  dispatcher:create_task(TaskSpec),
    FreeTask = db:get_task(node()),
    JobType = (db:get_job_info(FreeTask#task.job_id))#job.job_type,
    end_per_test_case().

task_allocation_test() ->
    init_per_test_case(),
    JobId = db:add_job({raytracer, 0}),
    TaskSpec = {JobId, split, "", 0},
    TaskId =  dispatcher:create_task(TaskSpec),
    dispatcher:get_task(node(), self()),
    receive
        {task_response, Task} ->
            ?assert(Task#task_tmp.task_id =:= TaskId),
            ?assertEqual(split, Task#task_tmp.task_type),
            ?assertEqual(JobId, Task#task_tmp.job_id);
        Msg ->
            chronicler:error(io_lib:format("Wrong message received: ~p", [Msg]))            
    end,
    end_per_test_case().

%% This test case is used to verify that request times out
%% when there is no free tasks, so taskFetcher can try again at later time.
timeout_test() ->
    init_per_test_case(),
    dispatcher:get_task(node(), self()),
    receive
        Msg ->
            io:format("Unexpected message received: ~p", [Msg])
        after 1000 ->
            ok
    end,
    end_per_test_case().

task_completed_test() ->
    init_per_test_case(),
    AssignedTask = {new_task, 1, map, 0},
    AId = db:add_task(AssignedTask),
    db:assign_task(AId, node()),
    ?assertEqual(assigned, db:get_task_state(AId)),
    dispatcher:report_task_done(AId),
    Status = db:get_task_state(AId),
    ?assertEqual(done, Status),
    end_per_test_case().

end_test() ->
    db:stop(),
    dispatcher:terminate(finished, []).
