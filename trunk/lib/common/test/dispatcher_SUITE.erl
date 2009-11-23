-module(dispatcher_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").
-include_lib("master/include/db.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
        task_allocation,
        timeout_test,
        task_completed,
        free_tasks
    ].

init_per_suite(Config) ->
    error_logger:tty(false),

    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    ok = application:start(master),

    db:create_tables(ram_copies),

    Config.

end_per_suite(_Config) ->
    db:stop(),

    application:stop(master),
    application:stop(ecg),
    application:stop(chronicler),
    application:stop(common),
    ok.

init_per_testcase(task_allocation, Config) ->
    JobId = dispatcher:add_job({raytracer, mapreduce, chabbrik, 0}),
    [{job, JobId} | Config];
init_per_testcase(task_completed, Config) ->
    JobId = dispatcher:add_job({raytracer, mapreduce, chabbrik, 0}),
    [{job, JobId} | Config];
init_per_testcase(free_tasks, Config) ->
    JobId = dispatcher:add_job({raytracer, mapreduce, chabbrik, 0}),
    [{job, JobId} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(task_allocation, Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    db:free_tasks(node()),
    db:remove_job(JobId),
    ok;
end_per_testcase(task_completed, Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    db:free_tasks(node()),
    db:remove_job(JobId),
    ok;
end_per_testcase(free_tasks, Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    db:free_tasks(node()),
    db:remove_job(JobId),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

task_allocation(Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),

    TaskSpec = {JobId, raytracer, split, "input_path"},
    TaskId =  dispatcher:add_task(TaskSpec),

    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, Task} ->
            TaskId = Task#task.task_id,
            split = Task#task.type,
            JobId = Task#task.job_id
    after 1000 ->
            ct:fail(timeout)
    end,
    ok.

%% This test case is used to verify that request times out
%% when there is no free tasks, so taskFetcher can try again at later time.
timeout_test(_Config) ->
    dispatcher:fetch_task(node(), self()),
    receive
        {task_response, no_task} -> ok
    after 2000 ->
            ct:fail(timeout)
    end.

task_completed(Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    dispatcher:add_task({JobId, raytracer, split, "/input/data.dat"}),
    dispatcher:fetch_task(node(), self()),
    receive
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
    ok.

free_tasks(Config) ->
    {job, JobId} = lists:keyfind(job, 1, Config),
    TaskId = dispatcher:add_task({JobId, raytracer, split, "/input/data.dat"}),
    ok = dispatcher:fetch_task(node(), self()),
    receive
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
    ok.

%%TODO fix this test below.

            %is this a separate test?
%            MapTask1 = {JobId, raytracer, map, "input data1"},
%            TaskId = dispatcher:add_task(MapTask1),

            %Expecting to get added tasks back
%            NewTask = db:fetch_task(node()),

 %           TaskId  = NewTask#task.task_id,
  %          JobId  = NewTask#task.job_id,
    %        raytracer  = NewTask#task.program_name,
   %         map  = NewTask#task.type,
     %       "input data1"  = NewTask#task.path,
      %      free  = NewTask#task.state,

       %     examiner:report_assigned(NewTask#task.job_id, NewTask#task.type),

            %Task2 = db:fetch_task(node()),
            %examiner:report_assigned(Task2#task.job_id, Task2#task.type),
            %no_task /= Task1,
            %no_task /= Task2,
