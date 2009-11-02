-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/db.hrl").

init_per_test_case() ->
    %error_logger:tty(true),
    db:start_link(test).

end_per_test_case() ->
    db:stop().

init_test() ->
    application:start(chronicler),
    ok.

db_test() ->
    init_test(),
    init_per_test_case(),
    % chronicler:set_logging_level(all),    
    % Check to see that the database doesn't blow up
    % just because there are no jobs and stuff
    ?assertEqual(no_task, db:fetch_task(node())),

    % Two jobs.
    JobA = {raytracer, mapreduce, ystadsnisse, 15},
    JobB = {wordcount, mapreduce, grillbritt, 30},
    
    % Let's add them
    JobAId = db:add_job(JobA),
    JobBId = db:add_job(JobB),
    
    ListOfUserJobs = db:get_user_jobs(grillbritt),
    ?assertEqual([JobBId], ListOfUserJobs),
    
    % Add one task, then fetch it
    Task1 = {JobAId, raytracer, split, 'ystads-nisse/pron'},
    Task1Id = db:add_task(Task1),
    ?assert(is_integer(Task1Id)),
    Task1Fetch = db:fetch_task(bongobongo),
    ?assertMatch(#task{}, Task1Fetch),
    ?assertEqual(Task1Id, Task1Fetch#task.task_id),
    
    % Try to fetch another task, should receive no_task
    Task2Fetch = db:fetch_task(mongomongo),
    ?assertEqual(no_task, Task2Fetch),
    
    % Make sure that JobA has its state set to 
    % no_tasks, since bongobongo has been assigned
    % the only task.
    ?assertEqual(no_tasks, (db:get_job(JobAId))#job.state),
    
    % Free all tasks associated to JobA and check that
    % its free
    db:free_tasks(bongobongo),
    ?assertEqual(free, (db:get_job(JobAId))#job.state),

    % Add one more task and try to fetch it
    Task2 = {JobAId, raytracer, map, 'ystads-nisse/pron'},
    Task2Id = db:add_task(Task2),

    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    db:fetch_task(bongobongo),
    ?assertEqual([], db:list(split_free)),
    ?assertEqual([], db:list(map_free)),
    ?assertEqual(1, length(db:list(split_assigned))),
    ?assertEqual(1, length(db:list(map_assigned))),

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
    
    ?assertEqual(1, length(db:list(reduce_free))),
    %?assertEqual(1, length(db:list(finalize_free))),

    ?assertEqual(free, (db:get_task(Task3Id))#task.state),
    ?assertEqual(no_tasks, (db:get_job(JobBId))#job.state),

    db:pause_job(JobAId),
    ?assertEqual(paused, (db:get_job(JobAId))#job.state),

    PausedFetchTask = db:fetch_task(mongomongo),
    ?assertEqual(no_task, PausedFetchTask),

    db:resume_job(JobAId),
    ?assertEqual(free, (db:get_job(JobAId))#job.state),

    db:stop_job(JobAId),
    ?assertEqual(stopped, (db:get_job(JobAId))#job.state),

    db:resume_job(JobAId),

    NoTask1 = db:fetch_task(bongobongo),
    NoTask2 = db:fetch_task(bongobongo),
    ?assertEqual(no_task, NoTask1),
    ?assertEqual(no_task, NoTask2),

    db:set_job_path(JobBId, Path = 'fudz/pr0n/lulz'),
    ?assertEqual(Path, (db:get_job(JobBId))#job.path),

    db:mark_done(Task1Id),
    Fetch1 = db:fetch_task(mongobongo),
    db:mark_done(Task2Id),

    ?assertEqual(no_task, Fetch1),
    ?assertEqual(done, (db:get_task(Task1Id))#task.state),
    ?assertEqual(1, length(db:list(map_done))),
    
    Fetch2 = db:fetch_task(mongobongo),
    Fetch3 = db:fetch_task(mongobongo),

    Fetch2Id = Fetch2#task.task_id,

    ?assert(lists:member(Fetch2Id, db:list(assigned_tasks))),
    ?assert(lists:member(Fetch2Id, db:list(reduce_assigned))),
    ?assertEqual(no_task, Fetch3),

    db:mark_done(Fetch2Id),

    Fetch4 = db:fetch_task(mongobongo),
    Fetch4Id = Fetch4#task.task_id,
    ?assert(lists:member(Fetch4Id, db:list(assigned_tasks))),
    ?assertEqual(1, length(db:list(finalize_assigned))),

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
    ?assertEqual([], MotherOfAllLists),

    end_per_test_case().

