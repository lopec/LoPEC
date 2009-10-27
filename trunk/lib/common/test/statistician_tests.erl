%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Tests for the statistician module. Was originally just
%%% a script for copypasting into erl console.
%%%
%%% @end
%%% Created : 23 Oct 2009 by Axel <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(statistician_tests).
-include_lib("eunit/include/eunit.hrl").

statistician_slave_test_() ->
    {setup,
     fun start_slave/0,
     fun stop_slave/1,
     fun (Pid) ->
             {inorder,
              [
               ?_assertNot(undefined == ets:info(stats)), % fail if table wasn't created
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               %Normally we'd wait for the flush, but in tests we're better
               %off doing it manually (and instantly)
               ?_assertEqual(flush, Pid ! flush), %flush when empty...
               ?_assertEqual(ok,
                             statistician:update({{1, 2, map},1,1,1,1,1,1})),
               ?_assertNot({error, no_such_job_in_stats} ==
                             statistician:get_job_stats(2)),
               ?_assertEqual(flush, Pid ! flush), %flush with 1 element...
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               ?_assertEqual(ok,
                             statistician:update({{2, 2, reduce},2,2,2,2,2,2})),
               ?_assertEqual(ok,
                             statistician:update({{1, 2, map},1,1,1,1,1,1})),
               ?_assertEqual(flush, Pid ! flush), %flush with multiple elements
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2))
               ]}
     end
     }.

statistician_master_test_() ->
    {setup,
     fun start_master/0,
     fun stop_master/1,
     fun (Pid) ->
             {inorder,
              [
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               ?_assertEqual(ok,
                             statistician:update({{1, 2, 3},a,b,c,d,e,f})),
               ?_assertNot({error, no_such_job_in_stats} ==
                           statistician:get_job_stats(2)),
               %job_finished (API function) requires waiting for ~3 seconds,
               %which we don't really want to do in tests. Thus, a direct call:
               ?_assertEqual({job_finished, 2}, Pid ! {job_finished, 2}),
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               ?_assertEqual(ok,
                             statistician:update({{1, 1, split},1,1,1,1,1,1})),
               ?_assertEqual(ok,
                             statistician:update({{20, 20, map},2,2,2,2,2,2})),
               ?_assertEqual(ok,
                             statistician:update({{3, 2, reduce},3,3,3,3,3,3})),
               ?_assertEqual(ok,
                             statistician:update({{4,2,finalize},2,2,2,2,2,2})),
               %Should add up to {{_,2,finalize},4,4,4,4,4}
               ?_assertEqual(ok,
                             statistician:update({{4,2,finalize},2,2,2,2,2,2})),
               %Unfortunately we cannot test that the numbers are correct, as
               %it would require checking against a 30-line string, and one of
               %the values in it (Time passed) cannot be known until runtime.
               ?_assertNot({error, no_such_job_in_stats} ==
                           statistician:get_job_stats(1)),
               ?_assertNot({error, no_such_job_in_stats} ==
                           statistician:get_job_stats(20)),
               ?_assertNot({error, no_such_job_in_stats} ==
                           statistician:get_job_stats(2)),
               ?_assertEqual({job_finished, 1}, Pid ! {job_finished, 1}),
               ?_assertEqual({job_finished, 20}, Pid ! {job_finished, 20}),
               ?_assertEqual({job_finished, 2}, Pid ! {job_finished, 2}),
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(1)),
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(20)),
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               %GARBAGE TESTS FOR 100% COVERAGE FOLLOW, MAY BE REMOVED FREELY
               ?_assertEqual(please_wait_a_few_seconds,
                             statistician:job_finished(1)),
               ?_assertEqual({noreply, []},
                             statistician:handle_call(aaa, self(), [])),
               ?_assertEqual({noreply, []},
                             statistician:handle_cast(bbb, [])),
               ?_assertEqual({noreply, []},
                             statistician:handle_info(ccc, [])),
               ?_assertEqual(ok,
                             statistician:terminate(foo, [])),
               ?_assertEqual({ok, []},
                             statistician:code_change(bar, [], baz))
              ]}
     end
     }.


start_master() ->
    application:start(chronicler),
    application:start(common),
    {ok, Pid} = statistician:start_link(master),
    Pid.

stop_master(_Pid) ->
    application:stop(chronicler),
    application:stop(common),
    statistician:stop().

start_slave() ->
    application:start(chronicler),
    application:start(common),
    {ok, Pid} = statistician:start_link(slave),
    Pid.

stop_slave(_Pid) ->
    application:stop(chronicler),
    application:stop(common),
    statistician:stop().

