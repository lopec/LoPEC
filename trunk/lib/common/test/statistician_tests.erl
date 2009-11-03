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
     fun ({Pid, Now, Node1, Node2}) ->
             {inorder,
              [
               ?_assertNot(undefined == ets:info(stats)), 
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now)),
               %Normally we'd wait for the flush, but in tests we're better
               %off doing it manually (and instantly)
               ?_assertEqual(flush, Pid ! flush), %flush when empty...
               ?_assertEqual(ok, statistician:update({{Node1, Now, map},
                                                      1, 1, 1, 1, 1, 1})),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_job_stats(Now)),
               ?_assertEqual(flush, Pid ! flush), %flush with 1 element...
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now)),
               ?_assertEqual(ok, statistician:update({{Node2 ,Now, reduce},
                                                      2, 2, 2, 2, 2, 2})),
               ?_assertEqual(ok, statistician:update({{Node1, Now, map},
                                                      1, 1, 1, 1, 1, 1})),
               ?_assertEqual(flush, Pid ! flush), %flush with multiple elements
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now))
               ]}
     end
     }.

statistician_master_test_() ->
    {setup,
     fun start_master/0,
     fun stop_master/1,
     fun ({Pid, {Now1, Now2, Now3}, {Node1, Node2, Node3, Node4}}) ->
             {inorder,
              [
               ?_assertEqual({error, no_stats_in_cluster},
                             statistician:get_cluster_stats()),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now1)),
               ?_assertEqual({error, no_such_node_in_stats},
                             statistician:get_node_stats(Node1)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_node_job_stats(Node1, Now1)),
               ?_assertEqual(ok, statistician:update({{Node1, Now1, 3},
                                                      0, 0, 0, 0, 0, 0})),
               ?_assertNot({error, no_stats_in_cluster} ==
                             statistician:get_cluster_stats()),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(Now1)),
               ?_assertNot({error, no_such_node_in_stats} ==
                           statistician:get_node_stats(Node1)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node1, Now1)),
               %job_finished (API function) requires waiting for ~3 seconds,
               %which we don't really want to do in tests. Thus, a direct call:
               ?_assertEqual({job_finished, Now1}, Pid ! {job_finished, Now1}),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now1)),
               %the stats table is cleared of the job, but the global stats
               %should remain unchanged
               ?_assertNot({error, no_such_node_in_stats} ==
                           statistician:get_node_stats(Node1)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node1, Now1)),
               ?_assertEqual(ok, statistician:update({{Node1, Now3, split},
                                                      1, 1, 1, 1, 1, 1})),
               ?_assertEqual(ok, statistician:update({{Node2, Now2, map},
                                                      2, 2, 2, 2, 2, 2})),
               ?_assertEqual(ok, statistician:update({{Node3, Now1, reduce},
                                                      3, 3, 3, 3, 3, 3})),
               ?_assertEqual(ok, statistician:update({{Node4, Now1, finalize},
                                                      2, 2, 2, 2, 2, 2})),
               %Should add up to {{_,Now1,finalize},4,4,4,4,4}
               ?_assertEqual(ok, statistician:update({{Node4, Now1, finalize},
                                                      2, 2, 2, 2, 2, 2})),
               %Unfortunately we cannot test that the numbers are correct, as
               %it would require checking against a 30-line string, and one of
               %the values in it (Time passed) cannot be known until runtime.
               %So we just make sure there is an entry.
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(Now3)),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(Now2)),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(Now1)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node1, Now3)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node2, Now2)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node3, Now1)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node4, Now1)),
               %More removing of jobs from stats table
               ?_assertEqual({job_finished, Now3}, Pid ! {job_finished, Now3}),
               ?_assertEqual({job_finished, Now2}, Pid ! {job_finished, Now2}),
               ?_assertEqual({job_finished, Now1}, Pid ! {job_finished, Now1}),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now3)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now2)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(Now1)),
               %Jobs are finished and removed, but node should remain...
               ?_assertNot({error, no_such_node_in_stats} ==
                           statistician:get_node_stats(Node1)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node1, Now3)),
               %...Until now!
               ?_assertEqual(ok, statistician:remove_node(Node1)),
               ?_assertEqual({error, no_such_node_in_stats},
                           statistician:get_node_stats(Node1)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_node_job_stats(Node1, Now3)),
               %GARBAGE TESTS
               %for 100% coverage, feel free to remove
               ?_assertEqual(please_wait_a_few_seconds,
                             statistician:job_finished(Now3)),
               ?_assertEqual({noreply, []},
                             statistician:handle_call(aaa, self(), [])),
               ?_assertEqual({noreply, []},
                             statistician:handle_cast(bbb, [])),
               ?_assertEqual({noreply, []},
                             statistician:handle_info(ccc, [])),
               ?_assertEqual(ok,
                             statistician:terminate(foo, [])),
               ?_assertEqual({ok, []},
                             statistician:code_change(bar, [], baz)),
               ?_assertEqual(flush, Pid ! flush)
               %because global statistician does not exist when testing flush in
               %slave, so we don't get to that part of the code without this
              ]}
     end
     }.


start_master() ->
    application:start(chronicler),
    application:start(common),
    Node1 = node(),
    Node2 = 'fakenode@10.10.10.10',
    Node3 = 'notrealnode@20.20.20.20',
    Node4 = 'mongo@10.20.30.40',
    {ok, Pid} = statistician:start_link(master),
    {Mega, Sec, Micro} = now(),
    Now1 = list_to_integer(lists:concat([Mega, Sec, Micro])),
    Now2 = Now1 + 23124,
    Now3 = Now1 - 14155,
    {Pid, {Now1, Now2, Now3}, {Node1, Node2, Node3, Node4}}.

stop_master({_Pid, {_Now1, _Now2, _Now3}, {_Node1, _Node2, _Node3, _Node4}}) ->
    application:stop(chronicler),
    application:stop(common),
    statistician:stop().

start_slave() ->
    application:start(chronicler),
    application:start(common),
    {ok, Pid} = statistician:start_link(slave),
    Node1 = node(),
    Node2 = 'lolnode@10.20.30.40',
    {Mega, Sec, Micro} = now(),
    Now = list_to_integer(lists:concat([Mega, Sec, Micro])),
    {Pid, Now, Node1, Node2}.

stop_slave({_Pid, _Now, _Node1, _Node2}) ->
    application:stop(chronicler),
    application:stop(common),
    statistician:stop().

