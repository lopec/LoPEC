-module(db_SUITE).

-include("../include/db.hrl").

-suite_defaults([{timetrap, {minutes, 10}}]).

-compile(export_all).

-includelib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    ok = application:start(master),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(master),
    ok = application:stop(ecg),
    ok = application:stop(chronicler),
    ok = application:stop(common),
    ok.

init_per_testcase(_TestCase, Config) ->
    [db:remove_job(Job) || Job <- db:list(job)],
    Config.

end_per_testcase(_TestCase, _Config) ->
    [db:remove_job(Job) || Job <- db:list(job)],
    ok.

all() ->
    [
        db_user_fetch,
        db_cancel_job,
        db_no_job,
        db_fetch_task,
        db_incorrect_input_type
    ].

db_user_fetch(_Config) ->
    % Two jobs.
    JobA1 = {raytracer, mapreduce, ystadsnisse, 15},
    JobA2 = {wordcount, mapreduce, ystadsnisse, 30},
    JobB1 = {raytracer, mapreduce, grillbritt, 30},
    JobB2 = {wordcount, mapreduce, grillbritt, 30},

    % Let's add them
    JobAId1 = db:add_job(JobA1),
    JobAId2 = db:add_job(JobA2),
    JobBId1 = db:add_job(JobB1),
    JobBId2 = db:add_job(JobB2),

    case db:get_user_jobs(grillbritt) of
        [JobBId1, JobBId2] ->
            ok;
        [JobBId2, JobBId1] ->
            ok
    end,
    
    case db:get_user_jobs(ystadsnisse) of
        [JobAId1, JobAId2] ->
            ok;
        [JobAId2, JobAId1] ->
            ok
    end.

db_no_task(_Config) ->
    no_task = db:fetch_task(node()).

db_cancel_job(_Config) ->
    JobC = {wordcount, mapreduce, skatasbort, 30},
    JobCId = db:add_job(JobC),
    %TODO Matcha!
    {_TaskID, []} = db:add_task({JobCId, raytracer, split, '/pr/stuff'}),
    {_TaskIS2, []} = db:fetch_task(skadellas),
    [skadellas] = db:cancel_job(JobCId).

db_no_job(_Config) ->
    TaskWithoutJob = {1, raytracer, split, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob),

    TaskWithoutJob2 = {1, raytracer, reduce, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob2).

db_fetch_task(_Config) ->
    Job = {raytracer, mapreduce, ystadsnisse, 15},
    JobId = db:add_job(Job),

    Task = {JobId, raytracer, split, 'ystads-nisse/pron'},
    {TaskId, []} = db:add_task(Task),
    true = is_integer(TaskId),

    %Fetch the task
    FetchLoop =
        fun
            (F, 0) ->
                F(F, no_task_found_in_fetch_loop);
            (F, N) ->
                case db:fetch_task(bongobongo) of
                    no_task -> F(F, N - 1);
                    Return -> Return
                end
        end,
    {TaskFetch, []} = FetchLoop(FetchLoop, 1000),
    #task{} = TaskFetch,

    %See that we got back the same task
    TaskId = TaskFetch#task.task_id,

    % Try to fetch another task, should receive no_task
    Task2Fetch = db:fetch_task(mongomongo),
    no_task = Task2Fetch.

db_incorrect_input_type(_Config) ->
    Job = {raytracer, mapreduce, ystadsnisse, 15},
    TaskWithErrorType = {Job, raytracer, non_existing_task_type, 'ystads-nisse/pron'},
    {error, incorrect_input_type} = db:add_task(TaskWithErrorType).

db_test() ->
    [{doc, "Test the WHOOOLE database."}].
db_test(_Config) ->
    JobAId = 1,
    JobBId = 2,
    Task1Id = 1,
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

