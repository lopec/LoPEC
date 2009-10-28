%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @author Vasilij Savin <vasilij.savin@gmail.com>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Contains the unit tests for the dispatcher. 
%%%
%%% @end
%%% Created : 1 Oct 2009 by Axel Andren <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(dispatcher_tests).
-include("../../master/include/db.hrl").
-include_lib("eunit/include/eunit.hrl").

end_per_test_case(JobId) ->
    db:free_tasks(node()),
    db:remove_job(JobId).

init_test() ->
    application:start(chronicler),
    application:start(ecg),
    dispatcher:start_link(),
    db:start_link(test).

task_allocation_test() ->
    JobId = dispatcher:add_job({raytracer, mapreduce, chabbrik, 0}),
    TaskSpec = {JobId, raytracer, split, "input_path"},
    TaskId =  dispatcher:add_task(TaskSpec),
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    %% The most buggy piece of cluster, unfortunately most critical too.
    %% Do not touch it, unles you REALLY know what you are doing.
    %% If something in cluster is not working, this is potentially reason.
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, Task} ->
            ?assert(Task#task.task_id =:= TaskId),
            ?assertEqual(split, Task#task.type),
            ?assertEqual(JobId, Task#task.job_id);
        Msg ->
            chronicler:error("Wrong message received: ~p", [Msg])  
        after 1000 ->
            chronicler:error("Task_alloc: Fetching timed out", [])
    end,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    end_per_test_case(JobId).

%% This test case is used to verify that request times out
%% when there is no free tasks, so taskFetcher can try again at later time.
timeout_test() ->
    dispatcher:fetch_task(node(), self()),
    receive
        Msg ->
            chronicler:error("Unexpected message received: ~p", [Msg])
        after 1000 ->
            ok
    end.

task_completed_test() ->
    JobId = dispatcher:add_job({ProgramName = raytracer, mapreduce, chabbrik, 0}),
    dispatcher:add_task({JobId, ProgramName, split, "/input/data.dat"}),
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, Task} ->
            ?assertEqual(split, Task#task.type),
            ?assertEqual(JobId, Task#task.job_id),
            ?assertEqual(assigned, (db:get_task(Task#task.task_id))#task.state),
            MapTaskSpec = {JobId, raytracer, map, "input data"},
            dispatcher:report_task_done(Task#task.task_id, MapTaskSpec),
            DoneTask = db:get_task(Task#task.task_id),
            ?assertEqual(done, DoneTask#task.state),
            AssignedTask = {JobId, raytracer, map, "input data"},
            AId = dispatcher:add_task(AssignedTask),
            ?assertEqual(free, (db:get_task(AId))#task.state);
        Msg ->
            chronicler:error("Wrong message received: ~p", [Msg])  
        after 1000 ->
            chronicler:error("Task_completed: Fetching timed out", [])
    end,
    end_per_test_case(JobId).

free_tasks_test() ->
    JobId = dispatcher:add_job({ProgramName = raytracer, mapreduce, chabbrik, 0}),
    dispatcher:add_task({JobId, ProgramName, split, "/input/data.dat"}),
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, Task} ->
            dispatcher:report_task_done(Task#task.task_id),
            MapTask1 = {JobId, raytracer, map, "input data1"},
            MapTask2 = {JobId, raytracer, map, "input data2"},
            dispatcher:add_task(MapTask1),
            dispatcher:add_task(MapTask2),
            %Expecting to get added tasks back
            Task1 = db:fetch_task(node()),
            Task2 = db:fetch_task(node()),
            chronicler:debug("~p: Found task1 ~p.~n",[?MODULE, Task1]),
            chronicler:debug("~p: Found task2 ~p.~n",[?MODULE, Task2]),
            ?assert(no_task /= Task1),
            ?assert(no_task /= Task2),
            chronicler:info("NodeId: ~p:", [node()]),
            dispatcher:free_tasks(node()),
            % We need to wait for cast to be processed.
            timer:sleep(100), 
            ?assertEqual([], db:list(map_assigned));
        Msg ->
            chronicler:error("Wrong message received: ~p", [Msg])  
        after 1000 ->
            chronicler:error("free_tasks: Fetching timed out", [])
    end,
    end_per_test_case(JobId).