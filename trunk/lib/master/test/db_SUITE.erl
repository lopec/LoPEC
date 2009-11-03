-module(db_SUITE).

-include("../include/db.hrl").

-suite_defaults([{timetrap, {minutes, 10}}]).

-compile(export_all).

-includelib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    Config.

end_per_suite(_Config) ->
    application:stop(chronicler),
    application:stop(common),
    ok.

init_per_testcase(_TestCase, Config) ->
    db:start_link(test),
    Config.

end_per_testcase(_TestCase, _Config) ->
    db:stop().

all() ->
    [db_test].

db_test() ->
    [{doc, "Test the WHOOOLE database."}].

db_test(_Config) ->
    % chronicler:set_logging_level(all),    
    % Check to see that the database doesn't blow up
    % just because there are no jobs and stuff
    no_task = db:fetch_task(node()),

    % Two jobs.
    JobA = {raytracer, mapreduce, ystadsnisse, 15},
    JobB = {wordcount, mapreduce, grillbritt, 30},
    
    % Let's add them
    JobAId = db:add_job(JobA),
    JobBId = db:add_job(JobB),
    
    ListOfUserJobs = db:get_user_jobs(grillbritt),
    [JobBId] = ListOfUserJobs,
    
    % Add one task, then fetch it
    Task1 = {JobAId, raytracer, split, 'ystads-nisse/pron'},
    Task1Id = db:add_task(Task1),
    true = is_integer(Task1Id),
    FetchLoop = fun (F, 0) -> olol = no_task_found_in_fetch_loop;
                    (F, N) -> case db:fetch_task(bongobongo) of
                                  no_task -> F(F, N - 1);
                                  Task = #task{} -> Task
                              end
                end,
    Task1Fetch = FetchLoop(FetchLoop, 1000),
    #task{} = Task1Fetch,
    Task1Id = Task1Fetch#task.task_id,
    
    % Try to fetch another task, should receive no_task
    Task2Fetch = db:fetch_task(mongomongo),
    no_task = Task2Fetch,
    
    % Make sure that JobA has its state set to 
    % no_tasks, since bongobongo has been assigned
    % the only task.
    no_tasks = (db:get_job(JobAId))#job.state,
    
    % Free all tasks associated to JobA and check that
    % its free
    db:free_tasks(bongobongo),
    free = (db:get_job(JobAId))#job.state,

    % Add one more task and try to fetch it
    Task2 = {JobAId, raytracer, map, 'ystads-nisse/pron'},
    Task2Id = db:add_task(Task2),

    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    [] = db:list(split_free),
    [] = db:list(map_free),
    [_OneSplitAss] = db:list(split_assigned),
    [_OneMapAss] = db:list(map_assigned),

    Task3 = {JobAId, raytracer, reduce, 'ystads-nisse/pron'},
    Task4 = {JobAId, raytracer, reduce, 'ystads-nisse/pron'},
    Task5 = {JobAId, raytracer, finalize, 'ystads-nisse/pron'},
    Task6 = {JobAId, raytracer, finalize, 'ystads-nisse/pron'},
    Task7 = {JobAId, ololol, ololol, 'ololol'},

    Task3Id = db:add_task(Task3),
    db:add_task(Task4),
    _Task5Id = db:add_task(Task5),
    db:add_task(Task6),
    db:add_task(Task7),
    
    [_OneReduceFree] = db:list(reduce_free),
    %?assertEqual(1, length(db:list(finalize_free))),

    free = (db:get_task(Task3Id))#task.state,
    no_tasks = (db:get_job(JobBId))#job.state,

    db:pause_job(JobAId),
    paused = (db:get_job(JobAId))#job.state,

    PausedFetchTask = db:fetch_task(mongomongo),
    no_task = PausedFetchTask,

    db:resume_job(JobAId),
    free = (db:get_job(JobAId))#job.state,

    db:stop_job(JobAId),
    stopped = (db:get_job(JobAId))#job.state,

    db:resume_job(JobAId),

    NoTask1 = db:fetch_task(bongobongo),
    NoTask2 = db:fetch_task(bongobongo),
    no_task = NoTask1,
    no_task = NoTask2,

    db:set_job_path(JobBId, Path = 'fudz/pr0n/lulz'),
    Path = (db:get_job(JobBId))#job.path,

    db:mark_done(Task1Id),
    Fetch1 = db:fetch_task(mongobongo),
    db:mark_done(Task2Id),

    no_task = Fetch1,
    done = (db:get_task(Task1Id))#task.state,
    [_OneMapDone] = db:list(map_done),

    Fetch2 = db:fetch_task(mongobongo),
    Fetch3 = db:fetch_task(mongobongo),

    Fetch2Id = Fetch2#task.task_id,

    true = lists:member(Fetch2Id, db:list(assigned_tasks)),
    true = lists:member(Fetch2Id, db:list(reduce_assigned)),
    no_task = Fetch3,

    db:mark_done(Fetch2Id),

    Fetch4 = db:fetch_task(mongobongo),
    Fetch4Id = Fetch4#task.task_id,
    true = lists:member(Fetch4Id, db:list(assigned_tasks)),
    [_OneFinalizeAss] = db:list(finalize_assigned),

    db:mark_done(Fetch4Id),

    db:remove_job(JobAId),
    db:remove_job(JobBId),

    MotherOfAllLists = lists:concat([db:list(split_free), 
				     db:list(split_assigned),
				     db:list(split_done),
				     db:list(map_free), 
				     db:list(map_assigned),
				     db:list(map_done),
				     db:list(reduce_free), 
				     db:list(reduce_assigned),
				     db:list(reduce_done),
				     db:list(finalize_free), 
				     db:list(finalize_assigned),
				     db:list(finalize_done),
				     db:list(assigned_tasks), 
				     db:list(task_relations),
				     db:list(job)]),
    [] = MotherOfAllLists.

