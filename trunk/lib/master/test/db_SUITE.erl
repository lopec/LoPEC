-module(db_SUITE).

-include("../include/db.hrl").

-suite_defaults([{timetrap, {minutes, 10}}]).

-compile(export_all).

-includelib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(ecg),
    ok = application:stop(chronicler),
    ok = application:stop(common),
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = db:start_link(test),
    Config.

end_per_testcase(_TestCase, _Config) ->
    [db:remove_job(Job) || Job <- db:list(job)],
    db:stop(),
    ok.

all() ->
    [
     db_add_get_remove_jobs,
     db_user_jobs,
     db_add_fetch_task,
     db_cancel_job,
     db_incorrect_input_type,
     db_bg_job_test,
     db_storage_keys
    ].

db_add_get_remove_jobs(Config) ->
    %Check that db is empty
    [] = db:list(job),
    
    %Two job definitions (the 1 is priority; currently unused)
    JobDef1 = {program1, problemtype1, user, 1},
    JobDef2 = {program2, problemtype2, user, 2},

    %Adding the definitions to the db...
    JobId1 = db:add_job(JobDef1),
    JobId2 = db:add_job(JobDef2),

    %Checking that the jobs were added
    false = ([] == db:list(job)),
    Job1 = db:get_job(JobId1),
    Job2 = db:get_job(JobId2),

    %Checking that the details match
    user         = Job1#job.owner,
    program1     = Job1#job.program_name,
    JobId1       = Job1#job.job_id,
    problemtype1 = Job1#job.problem_type,
    1            = Job1#job.priority,

    user         = Job2#job.owner,
    program2     = Job2#job.program_name,
    JobId2       = Job2#job.job_id,
    problemtype2 = Job2#job.problem_type,
    2            = Job2#job.priority,

    %Remove the jobs from db
    db:remove_job(JobId1),
    db:remove_job(JobId2),
    
    %Check that they were removed
    [] = db:list(job),
    
    Config.

db_user_jobs(Config) ->
    JobA1 = {program1, problemtype, user1, 1},
    JobA2 = {program2, problemtype, user1, 1},
    JobB1 = {program1, problemtype, user2, 1},
    JobB2 = {program2, problemtype, user2, 1},

    JobAId1 = db:add_job(JobA1),
    JobAId2 = db:add_job(JobA2),
    JobBId1 = db:add_job(JobB1),
    JobBId2 = db:add_job(JobB2),

    case db:get_user_jobs(user1) of
        [JobAId1, JobAId2] ->
            ok;
        [JobAId2, JobAId1] ->
            ok
    end,
    case db:get_user_jobs(user2) of
        [JobBId1, JobBId2] ->
            ok;
        [JobBId2, JobBId1] ->
            ok
    end,
    Config.

db_add_fetch_task(Config) ->
    Job = {program, problemtype, user, 1},
    JobId = db:add_job(Job),

    %Task definition
    Bucket = "path",
    Key = "filename",
    TaskDef = {JobId, program, split, {Bucket, Key}},
    %Add task to db, receive task_id
    {ok, {TaskId, _NodeToKill}} = db:add_task(TaskDef),
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
    TaskId        = TaskFetch#task.task_id,
    JobId         = TaskFetch#task.job_id,
    program       = TaskFetch#task.program_name,
    split         = TaskFetch#task.type,
    {Bucket, Key} = TaskFetch#task.path,
    free          = TaskFetch#task.state,
    
    %Try to fetch another task, should receive no_task
    no_task = db:fetch_task(mongomongo),
    Config.


db_cancel_job(Config) ->
    [] = db:list(job),
    %Can't check that running tasks are interrupted.
    %But a user story tests that for us
    Job = {program1, problemtype, user, 30},
    JobId = db:add_job(Job),
    
    false = ([] == db:list(job)),
    %TODO: Match!
    Bucket = "path",
    Key = "filename",
    {ok, {_TaskId, _NodeToKill}} =
        db:add_task({JobId, program2, split, {Bucket, Key}}),
    {_TaskIS2, []} = db:fetch_task(skadellas),
    [skadellas] = db:cancel_job(JobId),
    [] = db:list(job),
    Config.


db_no_job(Config) ->
    TaskWithoutJob = {1, program, split, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob),

    TaskWithoutJob2 = {1, program, reduce, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob2),
    Config.

db_incorrect_input_type(Config) ->
    Job = {program, problemtype, ystadsnisse, 15},
    TaskWithErrorType = {Job, program, non_existing_task_type,
                         'ystads-nisse/pron'},
    {error, incorrect_input_type} = db:add_task(TaskWithErrorType),
    Config.

db_bg_job_test(Config) ->
    Bucket1 = "bkt1",
    Bucket2 = "bkt2",
    Bucket3 = "bkt3",
    Bucket4 = "bkt4",
    Key = "key",
    
    %Add a bg job
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    %Add tasks for it
    {ok, {BGTaskId1, _NodeToKill}} =
        db:add_task({BGJobId, prog, split, {Bucket1, Key}}),
    {ok, {BGTaskId2, _NodeToKill}} =
        db:add_task({BGJobId, prog, split, {Bucket2, Key}}),
    %Check that the right (amount of) tasks were added
    true = no_task /= db:fetch_task(node_id_1),
    true = no_task /= db:fetch_task(node_id_2),
    false= no_task /= db:fetch_task(node_id_3),
    %Undoes the effects of the fetching of tasks
    db:free_tasks(node_id_1),
    db:free_tasks(node_id_2),

    %Add a regular job
    JobId = db:add_job({prog, type, user, prio}),

    %Add a task for it, and make sure it is not one of the existing BGTaskIds??
    case db:add_task({JobId, prog, split, {Bucket4, Key}}) of
        {ok, {TaskId1, []}} ->
            true = ((TaskId1 /= BGTaskId1) andalso (TaskId1 /= BGTaskId2)),
            true = (BGJobId  /= (db:get_task(TaskId1))#task.job_id)
    end,
    
    {ok, {_TaskId2, _NodeToKill}} =
        db:add_task({JobId, prog, reduce, {Bucket3, Key}}),
    %Fetch task - we expect to NOT get one of the bgjob tasks.
    case db:fetch_task(node_id_1) of
        no_task ->
            ct:fail("No tasks available.");
        {Task, []} ->
            JobId = Task#task.job_id;
        {_Task, NodesToKill} -> 
            ct:fail(io_lib:format("The list of nodes to kill was something "
                                  "other than the empty list: ~p",
                                  [NodesToKill]))
    end,
    Config.

db_storage_keys(Config) ->
    JobId = db:add_job({raytracer2, mapreduce, pwner, 9001}),
    true = is_integer(JobId),
    Bucket = list_to_binary(lists:concat([JobId, "/split/split1/"])),
    Key = <<"foo">>,
    {ok, {_TaskId, []}} =
        db:add_task({JobId, raytracer2, split, {Bucket, Key}}),
    {ok, [Key]} = db:list_keys(Bucket),
    Key2 = <<"bar">>,
    {ok, task_exists} =
        db:add_task({JobId, raytracer2, split, {Bucket, Key2}}),
    case db:list_keys(Bucket) of
        {ok, [Key, Key2]} -> ok;
        {ok, [Key2, Key]} -> ok
    end,
    Config.
