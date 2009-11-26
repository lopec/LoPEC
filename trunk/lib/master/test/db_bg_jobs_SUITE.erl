-module(db_bg_jobs_SUITE).

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
        bg_job_test_split,
        bg_job_test_map,
        bg_job_test_reduce,
        bg_job_test_finalize
    ].

bg_job_test_split() ->
    [{doc, "Test adding split when there are background jobs."}].
bg_job_test_split(_Config) ->
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    {BGTaskId1, []} = db:add_task({BGJobId, prog, split, "path1"}),
    {BGTaskId2, []} = db:add_task({BGJobId, prog, split, "path2"}),
    _BGTask1 = db:fetch_task(my_name_is1),
    _BGTask2 = db:fetch_task(my_name_is2),
    JobId = db:add_job({prog, type, user, prio}),
    case db:add_task({JobId, prog, split, "path3"}) of
        {TaskId, [One]} when One == my_name_is1 ; One == my_name_is2 ->
            true = (BGTaskId1 /= TaskId andalso BGTaskId2 /= TaskId)
    end.

bg_job_test_map() ->
    [{doc, "Test adding map when there are background jobs."}].
bg_job_test_map(_Config) ->
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    {BGTaskId1, []} = db:add_task({BGJobId, prog, split, "path1"}),
    {BGTaskId2, []} = db:add_task({BGJobId, prog, split, "path2"}),
    _BGTask1 = db:fetch_task(my_name_is1),
    _BGTask2 = db:fetch_task(my_name_is2),
    JobId = db:add_job({prog, type, user, prio}),
    case db:add_task({JobId, prog, map, "path3"}) of
        {TaskId, [One]} when One == my_name_is1 ; One == my_name_is2 ->
            true = (BGTaskId1 /= TaskId andalso BGTaskId2 /= TaskId)
    end.

bg_job_test_reduce() ->
    [{doc, "Test adding reduce when there are background jobs."}].
bg_job_test_reduce(_Config) ->
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    {_BGTaskId1, []} = db:add_task({BGJobId, prog, split, "path1"}),
    {_BGTaskId2, []} = db:add_task({BGJobId, prog, split, "path2"}),
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
            ololol
    end.

bg_job_test_finalize() ->
    [{doc, "Test adding finalize when there are background jobs."}].
bg_job_test_finalize(_Config) ->
    BGJobId = db:add_bg_job({prog, type, user, prio}),
    {_BGTaskId1, []} = db:add_task({BGJobId, prog, split, "path1"}),
    {_BGTaskId2, []} = db:add_task({BGJobId, prog, split, "path2"}),
    _BGTask1 = db:fetch_task(my_name_is1),
    _BGTask2 = db:fetch_task(my_name_is2),
    JobId = db:add_job({prog, type, user, prio}),
    {_Task1, []} = db:add_task({JobId, prog, finalize, "path2"}),
    {_Task2, []} = db:add_task({JobId, prog, finalize, "path3"}),
    {_Task3, []} = db:add_task({JobId, prog, finalize, "path4"}),
    case db:fetch_task(my_name_is3) of
        {_Task4, [my_name_is1, my_name_is2]} ->
            ololol;
        {_Task4, [my_name_is2, my_name_is1]} ->
            ololol
    end.
