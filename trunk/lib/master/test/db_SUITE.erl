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
     db_storage_keys,
     db_add_get_remove_jobs,
     db_user_jobs,
     db_add_fetch_task,
     db_cancel_job,
     db_incorrect_input_type,
     bg_job_test_tasktypes
    ].

db_add_get_remove_jobs(_Config) ->
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
    user = Job1#job.owner,
    program1 = Job1#job.program_name,
    JobId1 = Job1#job.job_id,
    problemtype1 = Job1#job.problem_type,
    1 = Job1#job.priority,

    user = Job2#job.owner,
    program2 = Job2#job.program_name,
    JobId2 = Job2#job.job_id,
    problemtype2 = Job2#job.problem_type,
    2 = Job2#job.priority,

    %Remove the jobs from db
    db:remove_job(JobId1),
    db:remove_job(JobId2),
    
    %Check that they were removed
    [] = db:list(job),
    
    ok.

db_user_jobs(_Config) ->
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
    ok.

db_add_fetch_task(_Config) ->
    Job = {program, problemtype, user, 1},
    JobId = db:add_job(Job),

    %Task definition
    TaskDef = {JobId, program, split, 'some/path'},
    %Add task to db, receive task_id
    {TaskId, []} = db:add_task(TaskDef),
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
    
    %Try to fetch another task, should receive no_task
    no_task = db:fetch_task(mongomongo).

%% -record(task, {
%%           task_id,
%%           job_id,
%%           program_name,
%%           type, % i.e. map, reduce, split, finalise
%%           path,
%%           state = free
%%          }). % [free | assigned | done]

db_cancel_job(_Config) ->
    [] = db:list(job),
    %Can't check that running tasks are interrupted.
    %But a user story tests that for us
    Job = {program1, problemtype, user, 30},
    JobId = db:add_job(Job),
    
    false = ([] == db:list(job)),
    %TODO: Match!
    {_TaskID, []} = db:add_task({JobId, program2, split, '/pr/stuff'}),
    {_TaskIS2, []} = db:fetch_task(skadellas),
    [skadellas] = db:cancel_job(JobId),
    [] = db:list(job).

db_no_job(_Config) ->
    TaskWithoutJob = {1, program, split, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob),

    TaskWithoutJob2 = {1, program, reduce, 'ystads-nisse/pron'},
    {error, job_not_in_db} = db:add_task(TaskWithoutJob2).

db_incorrect_input_type(_Config) ->
    Job = {program, problemtype, ystadsnisse, 15},
    TaskWithErrorType = {Job, program, non_existing_task_type, 'ystads-nisse/pron'},
    {error, incorrect_input_type} = db:add_task(TaskWithErrorType).

bg_job_test_tasktypes() ->
    [{doc, "Test adding split/map/reduce/finalize when there are background jobs."}].
bg_job_test_tasktypes(_Config) ->
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    {BGTaskId1, []} = db:add_task({BGJobId, prog, split, "path1"}),
    {BGTaskId2, []} = db:add_task({BGJobId, prog, split, "path2"}),
    _BGTask1 = db:fetch_task(my_name_is1),
    _BGTask2 = db:fetch_task(my_name_is2),
    JobId = db:add_job({prog, type, user, prio}),
    
    {_Task1, []} = db:add_task({JobId, prog, reduce, "path2"}),
    {_Task2, []} = db:add_task({JobId, prog, reduce, "path3"}),
    {_Task3, []} = db:add_task({JobId, prog, reduce, "path4"}),
    case db:fetch_task(my_name_is3) of
        {_Task4, [my_name_is1, my_name_is2]} ->
            ololol;
        {_Task4, [my_name_is2, my_name_is1]} ->
            ololol;
        {_Task4, []} ->
            ct:fail("Expected list of nodes to kill, got empty list.")
    end,
    db:free_tasks(my_name_is3),

    {_Task5, []} = db:add_task({JobId, prog, finalize, "path2"}),
    {_Task6, []} = db:add_task({JobId, prog, finalize, "path3"}),
    {_Task7, []} = db:add_task({JobId, prog, finalize, "path4"}),
    case db:fetch_task(my_name_is3) of
        {_Task8, [my_name_is1, my_name_is2]} ->
             ololol;
        {_Task8, [my_name_is2, my_name_is1]} ->
            ololol;
        {_Task8, []} ->
            ct:fail("Expected list of nodes to kill, got empty list.")
    end,
    
    case db:add_task({JobId, prog, split, "path3"}) of
        {TaskId1, [One]} when One == my_name_is1 ; One == my_name_is2 ->
            true = (BGTaskId1 /= TaskId1 andalso BGTaskId2 /= TaskId1)
    end,

    case db:add_task({JobId, prog, map, "path3"}) of
        {TaskId2, [Two]} when Two == my_name_is1 ; Two == my_name_is2 ->
            true = (BGTaskId1 /= TaskId2 andalso BGTaskId2 /= TaskId2)
    end,
	
    ok.

db_storage_keys() ->
    [{doc, "Test the storage key functions in db.erl"}].
db_storage_keys(_Config) ->
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
    ok.
