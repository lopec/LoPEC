%%%-------------------------------------------------------------------
%%% @author Burbas, Vasilij Savin <>
%%% @doc
%%%
%%% Contains the unit tests for the job listener. 
%%%
%%% @end
%%% Created : 14 Oct 2009 by Vasilij <>
%%%-------------------------------------------------------------------
-module(listener_tests).
-include("../include/db.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The way to run this test only, merge 3 lines below and run in console
%% erl -sname test 
%% -pa ../../common/ebin -pa ../../ecg/ebin -pa ../../logger/ebin -pa ../ebin 
%% -run listener_tests test
%% erl -sname server -pa common/ebin -pa ecg/ebin -pa logger/ebin -pa master/ebin
 
 job_name_test_() ->
     {setup,
        fun () -> 
            application:start(chronicler),
            chronicler:set_logging_level(all),
            application:start(ecg),
            application:start(common),
            listener:start_link(),
            dispatcher:start_link()
        end,
        fun (JobId) ->
            db:remove_job(JobId)
        end,
        fun () ->
            {ok, Root} = configparser:read_config("/etc/clusterbusters.conf", root),
            listener:add_job(raytracer, split, apanjansson, 0, Root)
        end
     }.

