%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% 
%%% @end
%%% Created : Oct 22, 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------

-module(examiner_SUITE).
-include("../include/global.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

init_per_suite(Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    Config.

end_per_suite(_Config) ->
    application:stop(chronicler),
    application:stop(common).

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = db:start_link(test),
    examiner:start_link(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    examiner:stop(),
    db:stop().

all() ->
    [report_test, out_of_bounds_test].

report_test() ->
    [{doc, "Test the progress reporting in examiner."}].
report_test(_Config) ->
    JobId = db:add_job({raytracer, mapreduce, chabbrik, 0}),
    examiner:insert(JobId),
    examiner:report_created(JobId, split),
    examiner:report_assigned(JobId, split),
    {0,1,0} = (examiner:get_progress(JobId))#job_stats.split,
    examiner:report_created(JobId, map),
    examiner:report_created(JobId, map),
    {2,0,0} = (examiner:get_progress(JobId))#job_stats.map,
    examiner:report_done(JobId, split),
    {0,0,1} = (examiner:get_progress(JobId))#job_stats.split,
    examiner:report_assigned(JobId, map),
    examiner:report_assigned(JobId, map),
    {0,2,0} = (examiner:get_progress(JobId))#job_stats.map,
    examiner:report_created(JobId, reduce),
    examiner:report_created(JobId, reduce),
    examiner:report_created(JobId, reduce),
    {3,0,0} = (examiner:get_progress(JobId))#job_stats.reduce,
    examiner:report_done(JobId, map),
    {0,1,1} = (examiner:get_progress(JobId))#job_stats.map,
    examiner:report_assigned(JobId, reduce),
    {2,1,0} = (examiner:get_progress(JobId))#job_stats.reduce,
    examiner:report_free([{JobId, map}, {JobId, reduce}]),
    {1,0,1} = (examiner:get_progress(JobId))#job_stats.map,
    {3,0,0} = (examiner:get_progress(JobId))#job_stats.reduce,

    {ok, JobId} = examiner:get_promising_job(),
    examiner:report_assigned(JobId, reduce),
    examiner:report_assigned(JobId, reduce),
    examiner:report_assigned(JobId, reduce),
    {0,3,0} = (examiner:get_progress(JobId))#job_stats.reduce,
    examiner:report_assigned(JobId, map),
    examiner:report_done(JobId, map),
    {0,0,2} = (examiner:get_progress(JobId))#job_stats.map,
    examiner:report_done(JobId, reduce),
    examiner:report_done(JobId, reduce),
    examiner:report_done(JobId, reduce),
    {0,0,3} = (examiner:get_progress(JobId))#job_stats.reduce,
    examiner:report_created(JobId, finalize),
    examiner:report_assigned(JobId, finalize),
    {0,1,0} = (examiner:get_progress(JobId))#job_stats.finalize,
    examiner:report_done(JobId, finalize).

out_of_bounds_test(_Config) ->
    {error, "There are no jobs."} = examiner:get_promising_job().
