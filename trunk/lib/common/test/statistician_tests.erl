%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Tests for the statistician module. It was originally just
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
     %What is 
     fun ({Pid, JobId, Node1, Node2}) ->
             {inorder,
              [
               ?_assertNot(undefined == ets:info(job_stats_table)), 
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId, raw)),
               %Normally we'd wait for the flush, but in tests we're better
               %off doing it manually (and instantly)
               ?_assertEqual(flush, Pid ! flush), %flush when empty...
               ?_assertEqual(ok, statistician:update({{Node1, JobId, map},
                                                      1.0, 1.0, 1, 1, 1, 1})),
               ?_assertEqual([1.0, 1.0, 1, 1, 1, 1],
                             statistician:get_job_stats(JobId, raw)),
               ?_assertEqual(flush, Pid ! flush), %flush with 1 element...
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId, raw)),
               ?_assertEqual(ok, statistician:update({{Node2 ,JobId, reduce},
                                                      2.0, 2.0, 2, 2, 2, 2})),
               ?_assertEqual(ok, statistician:update({{Node1, JobId, map},
                                                      1.0, 1.0, 1, 1, 1, 1})),
               ?_assertEqual([3.0, 3.0, 3, 3, 3, 3],
                             statistician:get_job_stats(JobId, raw)),
               ?_assertEqual(flush, Pid ! flush), %flush with multiple elements
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId, raw))
               ]}
     end
     }.

statistician_master_test_() ->
    {setup,
     fun start_master/0,
     fun stop_master/1,
     fun ({Pid, {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}) ->
             {inorder,
              [
               ?_assertEqual({[], [],0.0,0.0,0,0,0,0},
                             statistician:get_cluster_stats(raw)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId1, string)),
               ?_assertEqual({error, no_such_node_in_stats},
                             statistician:get_node_stats(Node1, string)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_node_job_stats(Node1, JobId1,
                                                             string)),
               ?_assertEqual(ok, statistician:update({{Node1, JobId1, split},
                                                      0.0, 0.0, 0, 0, 0, 0})),
               ?_assertEqual([0.0, 0.0, 0, 0, 0, 0],
                           statistician:get_job_stats(JobId1, raw)),
               ?_assertNot({[], [], 0.0,0.0,0,0,0,0} ==
                           statistician:get_cluster_stats(string)),
               ?_assertEqual({[Node1], [JobId1], 0.0,0.0,0,0,0,0},
                           statistician:get_cluster_stats(raw)),
               ?_assertEqual({Node1, [JobId1], 0.0,0.0,0,0,0,0},
                           statistician:get_node_stats(Node1, raw)),
               ?_assertEqual([0.0, 0.0, 0, 0, 0, 0],
                           statistician:get_node_job_stats(Node1, JobId1, raw)),
               %job_finished (API function) requires waiting for ~3 seconds,
               %which we don't really want to do in tests. Thus, a direct call:
               ?_assertEqual({job_finished, JobId1},
                             Pid ! {job_finished, JobId1}),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId1, raw)),
               %the stats table is cleared of the job, but the global stats
               %should remain unchanged
               ?_assertEqual({[Node1], [JobId1], 0.0,0.0,0,0,0,0},
                           statistician:get_cluster_stats( raw)),
               ?_assertEqual({Node1, [JobId1], 0.0,0.0,0,0,0,0},
                           statistician:get_node_stats(Node1, raw)),
               ?_assertEqual([0.0, 0.0, 0, 0, 0, 0],
                           statistician:get_node_job_stats(Node1, JobId1, raw)),
               ?_assertEqual([0.0, 0.0, 0, 0, 0, 0],
                             statistician:get_node_job_stats(Node1,JobId1,raw)),
               ?_assertEqual(ok, statistician:update({{Node1, JobId3, split},
                                                      1.0, 1.0, 1, 1, 1, 1})),
               ?_assertEqual(ok, statistician:update({{Node2, JobId2, map},
                                                      2.0, 2.0, 2, 2, 2, 2})),
               ?_assertEqual(ok, statistician:update({{Node3, JobId3, reduce},
                                                      3.0, 3.0, 3, 3, 3, 3})),
               ?_assertEqual(ok, statistician:update({{Node4, JobId1, finalize},
                                                      2.0, 2.0, 2, 2, 2, 2})),
               %Should update existing entry and add together, for a resulting
               %table entry of {{_,JobId1,finalize},4.0,4.0,4,4,4}
               ?_assertEqual(ok, statistician:update({{Node4, JobId1, finalize},
                                                      2.0, 2.0, 2, 2, 2, 2})),
               ?_assertEqual([4.0, 4.0, 4, 4, 4, 4],
                             statistician:get_job_stats(JobId1, raw)),
               ?_assertEqual([2.0, 2.0, 2, 2, 2, 2],
                             statistician:get_job_stats(JobId2, raw)),
               ?_assertEqual([4.0, 4.0, 4, 4, 4, 4],
                             statistician:get_job_stats(JobId3, raw)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_node_job_stats(Node1, JobId2,
                                                             raw)),
               ?_assertEqual([2.0, 2.0, 2, 2, 2, 2],
                             statistician:get_node_job_stats(Node2, JobId2,
                                                             raw)),
               ?_assertEqual([4.0, 4.0, 4, 4, 4, 4],
                             statistician:get_node_job_stats(Node4, JobId1,
                                                             raw)),
               %Unfortunately we cannot test that the numbers are correct when
               %asking for the formatted string, as this would require checking
               %against a 30-line string. What's more, one of the values in it
               %(Time passed) is unknown until runtime.
               %So we just make sure there is an entry, period.
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(JobId3, string)),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(JobId2, string)),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_job_stats(JobId1, string)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node1,JobId3,raw)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node2,JobId2,raw)),
               ?_assertNot({error, no_such_stats_found} ==
                             statistician:get_node_job_stats(Node4,JobId1,raw)),
               %More removing of jobs from stats table
               ?_assertEqual({job_finished, JobId1},
                             Pid ! {job_finished, JobId1}),
               ?_assertEqual({job_finished, JobId2},
                             Pid ! {job_finished, JobId2}),
               ?_assertEqual({job_finished, JobId3},
                             Pid ! {job_finished, JobId3}),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId1, raw)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId2, raw)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_job_stats(JobId3, raw)),
               %Jobs are finished and removed, but node should remain...
               ?_assertEqual({Node1, [JobId3, JobId1], 1.0, 1.0, 1, 1, 1, 1},
                           statistician:get_node_stats(Node1, raw)),
               ?_assertNot({error, no_such_node_in_stats} ==
                           statistician:get_node_stats(Node1, string)),
               ?_assertNot({error, no_such_stats_found} ==
                           statistician:get_node_job_stats(Node1, JobId3,
                                                           string)),
               ?_assertEqual([1.0, 1.0, 1, 1, 1, 1], 
                             statistician:get_node_job_stats(Node1,JobId3,raw)),
               %...Until now!
               ?_assertEqual(ok, statistician:remove_node(Node1)),
               ?_assertEqual({error, no_such_node_in_stats},
                           statistician:get_node_stats(Node1, raw)),
               ?_assertEqual({error, no_such_stats_found},
                             statistician:get_node_job_stats(Node1,JobId3,raw)),
               %GARBAGE TESTS
               %for 100% coverage, feel free to remove
               ?_assertEqual(please_wait_a_few_seconds,
                             statistician:job_finished(JobId3)),
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
    JobId1 = list_to_integer(lists:concat([Mega, Sec, Micro])),
    JobId2 = JobId1 + 23124,
    JobId3 = JobId1 - 14155,
    {Pid, {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}.

stop_master({_Pid, {_JobId1, _JobId2, _JobId3}, {_Node1, _Node2, _Node3, _Node4}}) ->
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
    JobId = list_to_integer(lists:concat([Mega, Sec, Micro])),
    {Pid, JobId, Node1, Node2}.

stop_slave({_Pid, _JobId, _Node1, _Node2}) ->
    application:stop(chronicler),
    application:stop(common),
    statistician:stop().
