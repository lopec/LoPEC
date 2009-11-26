-module(dispatcher_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").
-include_lib("../../master/include/db.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        task_allocation,
        timeout_test,
        task_completed,
        free_tasks,
        assigned_test
    ].

init_per_suite(Config) ->
    error_logger:tty(false),
    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    ok = application:start(master),

    Config.

end_per_suite(_Config) ->
    application:stop(master),
    application:stop(ecg),
    application:stop(chronicler),
    application:stop(common),
    ok.

init_per_testcase(_TestCase, Config) ->
    JobId = dispatcher:add_job({raytracer, mapreduce, chabbrik, 0}),
    [{job, JobId} | Config].

end_per_testcase(_TestCase, Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    db:free_tasks(node()),
    db:remove_job(JobId),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

task_allocation([{job, JobId} | Config]) ->
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ok;
        {task_response, _Task} ->
            ct:fail("fetch_task returned a task when expecting no_task!")
    after 1000 ->
            ct:fail(timeout)
    end,
    
    TaskSpec = {JobId, raytracer, split, "input_path"},
    TaskId   = dispatcher:add_task(TaskSpec),
    case TaskId of
        {error, Reason} ->
            ct:fail(Reason);
        _ ->
            ok    
    end,
    
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ct:fail("fetch_task returned no_task!");
        {task_response, Task} ->
            TaskId = Task#task.task_id,
            split  = Task#task.type,
            JobId  = Task#task.job_id
    after 1000 ->
            ct:fail(timeout)
    end,
    Config.

%% This test case is used to verify that request times out
%% when there is no free tasks, so taskFetcher can try again at later time.
timeout_test(Config) ->
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ok;
        {task_response, _Task} ->
            ct:fail("fetch_task returned a task when expecting no_task!")
    after 2000 ->
            Config
    end.

task_completed([{job, JobId} | Config]) ->
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ok;
        {task_response, _Task} ->
            ct:fail("fetch_task returned a task when expecting no_task!")
    after 1000 ->
            ct:fail(timeout)
    end,
    
    dispatcher:add_task({JobId, raytracer, split, "/input/data.dat"}),
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ct:fail("fetch_task returned no_task!");
        {task_response, Task} ->
            split = Task#task.type,
            JobId = Task#task.job_id,
            assigned = (db:get_task(Task#task.task_id))#task.state,
            MapTaskSpec = {JobId, raytracer, map, "input data"},
            dispatcher:report_task_done(Task#task.task_id, MapTaskSpec),
            DoneTask = db:get_task(Task#task.task_id),
            done = DoneTask#task.state,
            AssignedTask = {JobId, raytracer, map, "input data"},
            AId = dispatcher:add_task(AssignedTask),
            free = (db:get_task(AId))#task.state
    after 1000 ->
            ct:fail(timeout)
    end,
    Config.

free_tasks([{job, JobId} | Config]) ->
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ok;
        {task_response, _Task} ->
            ct:fail("fetch_task returned a task when expecting no_task!")
    after 1000 ->
            ct:fail(timeout)
    end,
    
    TaskId = dispatcher:add_task({JobId, raytracer, split, "/input/data.dat"}),
    ok = dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ct:fail("fetch_task returned no_task!");
        {task_response, Task} ->
            TaskId = Task#task.task_id,
            ok = dispatcher:report_task_done(Task#task.task_id),
            ok = dispatcher:free_tasks(node()),

            % We need to wait for cast to be processed.
            timer:sleep(100),
            [] = db:list(map_assigned)
    after 1000 ->
            ct:fail(timeout)
    end,
    Config.

assigned_test([{job, JobId} | Config]) ->
    MapTask1 = {JobId, raytracer, map, "input data1"},
    TaskId = dispatcher:add_task(MapTask1),

    %Expecting to get added tasks back,
    %If the matches below fails the database was not clean
    %Expected state - free
    NewTask = db:get_task(TaskId),

    TaskId        = NewTask#task.task_id,
    JobId         = NewTask#task.job_id,
    raytracer     = NewTask#task.program_name,
    map           = NewTask#task.type,
    "input data1" = NewTask#task.path,
    free          = NewTask#task.state,

    examiner:report_created(NewTask#task.job_id, NewTask#task.type),

    ok = dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} ->
            ct:fail("fetch_task returned no_task!");
        {task_response, AssignedTask} ->
            RetrievedTask = db:get_task(TaskId),
            AssignedTask#task.task_id == RetrievedTask#task.task_id,
            AssignedTask#task.job_id  == RetrievedTask#task.job_id,
            assigned = RetrievedTask#task.state,

            examiner:report_assigned(AssignedTask#task.job_id, AssignedTask#task.type)
    after 1000 ->
            ct:fail(timeout)
    end,
    Config.
