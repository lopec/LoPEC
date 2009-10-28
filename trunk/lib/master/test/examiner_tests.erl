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

%% -export([get_promising_job/0, get_progress/1, insert/1, remove/1, start_link/0,
%%          report_created/2, report_assigned/2, report_done/2, report_free/1]).


report_test() ->
    JobId = 123456,
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
    examiner:report_free([{JobId, map, 1}, {JobId, reduce, 1}]),
    ?assertEqual({1,0,1}, (examiner:get_progress(JobId))#job_stats.map),
    ?assertEqual({3,0,0}, (examiner:get_progress(JobId))#job_stats.reduce),
    examiner:remove(JobId).
