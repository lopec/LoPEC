%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% 
%%% @end
%%% Created : Oct 22, 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------

-module(examiner_tests).
-include("../include/global.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([]).

init_test() ->
    case whereis(db_server) of
        undefined -> db:start_link(test);
        _ -> ok
    end.

report_test() ->
    JobId = db:add_job({raytracer, mapreduce, chabbrik, 0}),
    examiner:start_link(),
    examiner:insert(JobId),
    ?assertEqual({0,0,0}, (examiner:get_progress(JobId))#job_stats.split),
    examiner:report_created(JobId, split),
    ?assertEqual({1,0,0}, (examiner:get_progress(JobId))#job_stats.split),
    examiner:report_assigned(JobId, split),
    ?assertEqual({0,1,0}, (examiner:get_progress(JobId))#job_stats.split),
    examiner:report_created(JobId, map),
    ?assertEqual({1,0,0}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_created(JobId, map),
    ?assertEqual({2,0,0}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_done(JobId, split),
    ?assertEqual({0,0,1}, (examiner:get_progress(JobId))#job_stats.split),
    examiner:report_assigned(JobId, map),
    ?assertEqual({1,1,0}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_assigned(JobId, map),
    ?assertEqual({0,2,0}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_created(JobId, reduce),
    ?assertEqual({1,0,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_created(JobId, reduce),
    ?assertEqual({2,0,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_created(JobId, reduce),
    ?assertEqual({3,0,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_done(JobId, map),
    ?assertEqual({0,1,1}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_assigned(JobId, reduce),
    ?assertEqual({2,1,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_free([{JobId, map}, {JobId, reduce}]),
    ?assertEqual({1,0,1}, (examiner:get_progress(JobId))#job_stats.map),
    ?assertEqual({3,0,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    ?assertEqual({ok, JobId}, examiner:get_promising_job()),
    examiner:report_assigned(JobId, reduce),
    ?assertEqual({2,1,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_assigned(JobId, reduce),
    ?assertEqual({1,2,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_assigned(JobId, reduce),
    ?assertEqual({0,3,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_assigned(JobId, map),
    ?assertEqual({0,1,1}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_done(JobId, map),
    ?assertEqual({0,0,2}, (examiner:get_progress(JobId))#job_stats.map),
    examiner:report_done(JobId, reduce),
    ?assertEqual({0,2,1}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_done(JobId, reduce),
    ?assertEqual({0,1,2}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_done(JobId, reduce),
    ?assertEqual({0,0,3}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:report_created(JobId, finalize),
    ?assertEqual({1,0,0}, (examiner:get_progress(JobId))#job_stats.finalize),
    examiner:report_assigned(JobId, finalize),
    ?assertEqual({0,1,0}, (examiner:get_progress(JobId))#job_stats.finalize),
    examiner:report_done(JobId, finalize).
    %examiner:remove(JobId).

out_of_bounds_test() ->
    examiner:start_link(),
    ?assertEqual({error, "There are no jobs."}, examiner:get_promising_job()).

stop_test() ->
    db:stop().
