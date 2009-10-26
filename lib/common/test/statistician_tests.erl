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

statistician_test_() ->
    {setup,
     fun start_master/0,
     fun stop_master/1,
     fun (Pid) ->
             {inorder,
              [
               ?_assertEqual({error, no_such_job_in_stats},
                             statistician:get_job_stats(2)),
               ?_assertEqual(ok, statistician:update({{1, 2, 3},a,b,c,d,e})),
               ?_assertNot({error, no_such_job_in_stats} ==
                             statistician:get_job_stats(2)),
               %job_finished (API function) requires waiting for ~3 seconds,
               %which we can't really do in tests. Thus the direct call.
               ?_assertMatch({noreply, []}, 
                            statistician:handle_info({job_finished,1}, [])),
                ?_assertEqual({error, no_such_job_in_stats},
                              statistician:get_job_stats(2)),
               ?_assertEqual(ok,
                             statistician:update({{1, 2, split},1,1,1,1,1})),
               ?_assertEqual(ok,
                             statistician:update({{2, 2, map},2,2,2,2,2})),
               ?_assertEqual(ok,
                             statistician:update({{3, 2, reduce},3,3,3,3,3})),
               ?_assertEqual(ok,
                             statistician:update({{4, 2, finalize},2,2,2,2,2})),
               %Should add up to 4 in each field
               ?_assertEqual(ok,
                             statistician:update({{4, 2, finalize},2,2,2,2,2})),
               ?_assertEqual(flush, Pid ! flush),
               ?_assertNot({error, no_such_job_in_stats} ==
                             statistician:get_job_stats(2))
               %Cannot test that the numbers are correct, as it would require
               %checking against a 30-line string, and one of the values in it
               %cannot be known until runtime due to being time-based.
               

               ]}
     end
     }.




start_master() ->
    application:start(chronicler),
    application:start(common),
    {ok, Pid} = statistician:start_link(master),
    Pid.

stop_master(_Pid) ->
    statistician:stop().

start_slave() ->
    {ok, Pid} = statistician:start_link(slave),
    Pid.

stop_slave(_Pid) ->
    ok.

