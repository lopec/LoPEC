-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/db.hrl").

init_per_test_case() ->
    error_logger:tty(true),
    db:start(test).

end_per_test_case() ->
    db:stop().

init_test() ->
    application:start(chronicler).

job_test() ->
    init_per_test_case(),
    JobTuple = {Program = raytracer, ProblemType = mapreduce, 
                Owner = owner, Priority = 15},
    JobId = db:add_job(JobTuple),
    Job = db:get_job(JobId),
    ?assertEqual(JobId, Job#job.job_id),
    ?assertEqual(Program, Job#job.program_name),
    ?assertEqual(ProblemType, Job#job.problem_type),
    ?assertEqual(Owner, Job#job.owner),
    ?assertEqual(Priority, Job#job.priority),
    ?assertMatch([_A], db:list(job)),
    end_per_test_case().    

task_test() ->
    init_per_test_case(),
    chronicler:set_logging_level(all),
    JobTuple = {Program = raytracer, ProblemType = mapreduce, 
                Owner = owner, Priority = 15},
    JobId = db:add_job(JobTuple),
    SplitTaskSpec = {JobId, raytracer, split, SplitPath = "./din_pappa"},
    SplitTaskId = db:add_task(SplitTaskSpec),
    ?assertEqual(free, (db:get_task(SplitTaskId))#task.state),
    SplitTask = db:fetch_task(node()),
    chronicler:info("DB: Spl Id from record ~p", [SplitTask]),
    ?assertEqual(assigned, (db:get_task(SplitTask#task.task_id))#task.state),
    io:format("DB: Spl from db by Id ~p", [db:get_task(SplitTask#task.task_id)]),
    ?assertEqual(SplitPath, SplitTask#task.path),
    db:mark_done(SplitTaskId),
    ?assertEqual(done, (db:get_task(SplitTaskId))#task.state),
    %TODO refactor into comprehensions
    %% Do not allow marking as done, if task was not assigned.
    MapTask1Spec = {JobId, raytracer, map, './map/file1'},
    MapTask2Spec = {JobId, raytracer, map, './map/file2'},
    MapTask3Spec = {JobId, raytracer, map, './map/file3'},
    MapTask1Id = db:add_task(MapTask1Spec),
    _MapTask2Id = db:add_task(MapTask2Spec),
    _MapTask3Id = db:add_task(MapTask3Spec),
    MapTask = db:fetch_task(node()),
    ?assertEqual(assigned, (db:get_task(MapTask#task.task_id))#task.state),
    ?assertEqual(map, MapTask#task.type),
    db:mark_done(MapTask1Id),
    ReduceTaskSpec = {JobId, raytracer, reduce, './reduce/file1'},
    _ReduceTaskId = db:add_task(ReduceTaskSpec),
    NextTask = db:fetch_task(node()),
    %We expect to get maps before reduces
    ?assertEqual(map, NextTask#task.type),
    %There should be no finalise tasks 
    FinalisedTasks = lists:concat([db:list(finalize_free), 
                  db:list(finalize_assigned), 
                  db:list(finalize_done)]),
    ?assertMatch([], FinalisedTasks),
    end_per_test_case.