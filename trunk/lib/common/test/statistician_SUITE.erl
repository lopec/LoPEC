%%%-------------------------------------------------------------------
%%% @author Axel Andrén <axelandren@gmail.com>
%%% @copyright (C) 2009, Axel Andrén
%%% @doc
%%%
%%% The CommonTest suite for statistician
%%%
%%% @end
%%% Created : 23 Nov 2009 by Axel Andrén <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(statistician_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     ets_exists,
     mem_and_disk,
     checkempty,
     update_and_getters,
     slavetests,
     jobfinished_and_removenode
    ].

init_per_suite(Config) ->
    error_logger:tty(false),
    ok = application:start(common),
    ok = application:start(chronicler),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(common),
    ok = application:stop(chronicler),
    ok.

%These first two are separate so some functions that the other testing methods
%use, can be tested before they are actually used.
init_per_testcase(ets_exists, Config) ->
    {ok, _Pid} = statistician:start_link(master),
    Config;
init_per_testcase(mem_and_disk, Config) ->
    Self = self(),
    Node = node(),
    {Mega, Sec, Micro} = now(),
    JobId = list_to_integer(lists:concat([Mega, Sec, Micro])),
    {ok, _Pid} = statistician:start_link(master),
    [{Self, Node, JobId} | Config];
init_per_testcase(slavetests, Config) ->
    {ok, Pid} = statistician:start_link(slave),
    Node1 = node(),
    {Mega, Sec, Micro} = now(),
    JobId1 = list_to_integer(lists:concat([Mega, Sec, Micro])),
    [{Pid, JobId1, Node1}
     | Config];
init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = statistician:start_link(master),
    ClusterDisk = statistician:get_cluster_disk_usage(raw),
    ClusterMem = statistician:get_cluster_mem_usage(raw),
    Node1 = node(),
    Node2 = 'fakenode@10.10.10.10',
    Node3 = 'notrealnode@20.20.20.20',
    Node4 = 'mongo@10.20.30.40',
    {Mega, Sec, Micro} = now(),
    JobId1 = list_to_integer(lists:concat([Mega, Sec, Micro])),
    JobId2 = JobId1 + 23124,
    JobId3 = JobId1 - 14155,
    [{Pid, ClusterDisk, ClusterMem,
     {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}
     | Config].


end_per_testcase(_TestCase, _Config) ->
    statistician:stop(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     test functions    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

ets_exists(Config) ->
    true = ets:info(job_stats_table) =/= undefined,
    Config.

mem_and_disk([{Self, Node, JobId} | Config]) ->
    statistician:update({{Node, JobId ,split, usr}, 0.0, 0.0, 0, 0, 0, 0,
                         {0,0},{0,0,{Self,0}}}),
    
    {NMTotalSize, NMTotalPercentage, {_NMWorstPid, NMWorstSize}} =
        statistician:get_node_mem_usage(raw),
    {NDTotalSize, NDTotalPercentage} =
        statistician:get_node_disk_usage(raw),
    [{per_node, _}, {collected,
                     [CMTotalSize, CMTotalUsed, CMAverageSize,
                      CMTotalPercentage, CMAveragePercentage]}] =
        statistician:get_cluster_mem_usage(raw),
    [{per_node, _}, {collected,
                     [CDTotalSize, CDTotalUsed, CDAverageSize,
                      CDTotalPercentage, CDAveragePercentage]}] =
        statistician:get_cluster_disk_usage(raw),

    %Values cannot be known beforehand, so we just check that they are in bounds.
    true = (NMTotalSize >= 0),
    true = (NMTotalPercentage >= 0),
    true = (NMTotalPercentage =< 100),
    true = (NMWorstSize >= 0),

    true = (NDTotalSize >= 0),
    true = (NDTotalPercentage >= 0),
    true = (NDTotalPercentage =< 100),

    true = (CMTotalSize >= 0),
    true = (CMTotalUsed >= 0),
    true = (CMTotalPercentage >= 0),
    true = (CMTotalPercentage =< 100),
    true = (CMAverageSize >= 0),
    true = (CMAveragePercentage >= 0),
    true = (CMAveragePercentage =< 100),

    true = (CDTotalSize >= 0),
    true = (CDTotalUsed >= 0),
    true = (CDTotalPercentage >= 0),
    true = (CDTotalPercentage =< 100),
    true = (CDAverageSize >= 0),
    true = (CDAveragePercentage >= 0),
    true = (CDAveragePercentage =< 100),

    %Can't easily check the output of strings, so we just make sure they
    %really are strings.
    true = is_list(statistician:get_node_mem_usage(string)),
    true = is_list(statistician:get_node_disk_usage(string)),
    true = is_list(statistician:get_cluster_mem_usage(string)),
    true = is_list(statistician:get_cluster_disk_usage(string)),
    
  %%   {_, _, {Self, _}} =
%%         statistician:get_node_mem_usage(raw),
%%     {_, _} =
%%         statistician:get_node_disk_usage(raw),
%%     [{per_node, _}, {collected,
%%                      [CMTotalSize, CMTotalUsed,
%%                       CMAverageSize, CMTotalPercentage, CMAveragePercentage]}] =
%%         statistician:get_cluster_mem_usage(raw),
%%     [{per_node, _}, {collected,
%%                      [CDTotalSize, CDTotalUsed, CDAverageSize,
%%                       CDTotalPercentage, CDAveragePercentage]}] =
%%         statistician:get_cluster_disk_usage(raw),
    Config.


checkempty([{_Pid, ClusterDisk, ClusterMem,
             {JobId1, _JobId2, _JobId3}, {Node1, _Node2, _Node3, _Node4}}
            | Config]) ->
    %Stats tables should be empty. For the cluster this means default
    %values of 0, but the others should return error tuples.
    {[], [],0.0,0.0,0,0,0,0,ClusterDisk,ClusterMem} =
        statistician:get_cluster_stats(raw),
    true = is_list(statistician:get_cluster_stats(string)),
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId1, raw),
    {error, no_such_user} =
        statistician:get_user_stats('_', raw),
    {error, no_such_node_in_stats} =
        statistician:get_node_stats(Node1, raw),
    {error, no_such_stats_found} =
        statistician:get_node_job_stats(Node1, JobId1, raw),
    {error, no_such_user} =
        statistician:get_user_stats('_', string),
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId1, string),
    {error, no_such_node_in_stats} =
        statistician:get_node_stats(Node1, string),
    {error, no_such_stats_found} =
        statistician:get_node_job_stats(Node1, JobId1, string),
    Config.

update_and_getters([{Pid, _ClusterDisk, _ClusterMem,
                     {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}
                    | Config]) ->

    Self = self(),

    %Lets add some fake info!
    statistician:update({{Node1, JobId1, split, usr}, 0.0, 0.0, 0, 0, 0, 0,
                         {0,0},{0,0,{Self,0}}}),
    statistician:update({{Node1, JobId3, split, usr}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{Self,1}}}),
    statistician:update({{Node2, JobId2, map, olololo}, 2.0, 2.0, 2, 2, 2, 2,
                         {2,2},{2,2,{Self,2}}}),
    statistician:update({{Node3, JobId3, reduce, usr}, 3.0, 3.0, 3, 3, 3, 3,
                         {3,3},{3,3,{Self,3}}}),
    statistician:update({{Node4, JobId1, finalize, usr}, 2.0, 2.0, 2, 2, 2, 2,
                         {2,2},{2,2,{Self,2}}}),
    %Should update existing entry and add together, for a resulting
    %table entry of {{_,JobId1,finalize, usr},4.0,4.0,4,4,4,{4,4},{4,4}}
    statistician:update({{Node4, JobId1, finalize, usr}, 2.0, 2.0, 2, 2, 2, 2,
                         {4,4},{4,4,{Self,4}}}),

    %Check if the update went as expected
    [4.0, 4.0, 4, 4, 4, 4] =
        statistician:get_job_stats(JobId1, raw),
    [0.0, 0.0, 0, 0, 0, 0] =
        statistician:get_node_job_stats(Node1, JobId1, raw),
    
    {{Node1}, Node1JobsList, 1.0,1.0,1,1,1,1,{1,1},{1,1,{Self,1}}} =
        statistician:get_node_stats(Node1, raw),
    %check that stats say node has worked on the expected jobs...
    true = lists:member(JobId1, Node1JobsList),
    true = lists:member(JobId3, Node1JobsList),
    %...and ONLY those jobs
    [] = Node1JobsList -- [JobId1, JobId3],
    
    {usr, {UsrJobsList, 8.0, 8.0, 8, 8, 8, 8}} =
        statistician:get_user_stats(usr, raw),
    true = lists:member(JobId1, UsrJobsList),
    true = lists:member(JobId3, UsrJobsList),
    [] = lists:subtract(UsrJobsList, [JobId1, JobId3]),

    %last argument here would normally be ClusterMem, but
    %memory used changes while testing...
    {Nodes, JobIds, 10.0,10.0,10,10,10,10,_,_} =
        statistician:get_cluster_stats(raw),
    true = lists:member(JobId1, JobIds),
    true = lists:member(JobId2, JobIds),
    true = lists:member(JobId3, JobIds),
    true = lists:member(Node1, Nodes),
    true = lists:member(Node2, Nodes),
    true = lists:member(Node3, Nodes),
    true = lists:member(Node4, Nodes),
    [] = lists:subtract(JobIds, [JobId1, JobId2, JobId3]), 
    [] = lists:subtract(Nodes, [Node1, Node2, Node3, Node4]),
    
    %Can't easily check output of strings...
    true = is_list(statistician:get_job_stats(JobId1, string)),
    true = is_list(statistician:get_user_stats(usr, string)),
    true = is_list(statistician:get_node_job_stats(Node1, JobId1, string)),
    true = is_list(statistician:get_node_stats(Node1, string)),
    true = is_list(statistician:get_cluster_stats(string)),

    %flushing here should result in the master sending all its data to itself
    %with a cast (which it'll handle later) and then clearing its table.
    Pid ! flush,

    %If it handles the cast immediately after, these stats should be in the
    %master stats again.
    timer:sleep(100),
    [4.0, 4.0, 4, 4, 4, 4] =
        statistician:get_job_stats(JobId1, raw),
    [0.0, 0.0, 0, 0, 0, 0] =
        statistician:get_node_job_stats(Node1, JobId1, raw),
    
    %Node stats should have been updated, doubling all job values
    %strangely the disk and mem seems to vary between being 0 or 1, so no match
    {{Node1}, Node1JobsList, 2.0,2.0,2,2,2,2,_Disk,_Mem} =
        statistician:get_node_stats(Node1, raw),
    true = lists:member(JobId1, Node1JobsList),
    true = lists:member(JobId3, Node1JobsList),
    [] = Node1JobsList -- [JobId1, JobId3],
    
    Config.


slavetests([{Pid, JobId, Node}
            | Config]) ->
    %must be empty initially
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId, raw),
    %flush when empty
    Pid ! flush,

    %Lets add some fake info!
    statistician:update({{Node, JobId, split, usr}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{self(),1}}}),
    %Was it added properly?
    [1.0, 1.0, 1, 1, 1, 1] =
        statistician:get_job_stats(JobId, raw),
    
    %Flush now should remove it. Though normally there'd be a wait and a msg
    %sent automatically rather than manually.
    Pid ! flush,
    %make sure it was removed
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId, raw),
    
    statistician:update({{Node, JobId, split, usr}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{self(),1}}}),
    statistician:update({{Node, JobId, split, usr}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{self(),1}}}),
    JobId2 = JobId + 12345,
    statistician:update({{Node, JobId2, map, usr}, 3.0, 3.0, 3, 3, 3, 3,
                         {3,3},{3,3,{self(),3}}}),

    %should add up the two updates
    [2.0, 2.0, 2, 2, 2, 2] =
        statistician:get_job_stats(JobId, raw),
    %flush with multiple elements
    Pid ! flush,
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId, raw),
    Config.


jobfinished_and_removenode([{_Pid, _ClusterDisk, _ClusterMem,
               {JobId1, _JobId2, _JobId3}, {Node1, _Node2, _Node3, _Node4}}
              | Config]) ->

    %Lets add some fake info!
    statistician:update({{Node1, JobId1, split, usr}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{self(),1}}}),

    %And then remove it. Normally done by calling the API function
    %job_finished/1, but that involves a 2+-second wait, which we do not want
    %to do in a testing environment.
    statistician:handle_info({job_finished, JobId1}, []),

    %Lets make sure it's  gone...
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId1, raw),
    {error, no_such_stats_found} =
        statistician:get_node_job_stats(Node1, JobId1, raw),

    %But not from the node stats!
    Self = self(),
    {{Node1}, [JobId1], 1.0, 1.0, 1, 1, 1, 1,{1,1},{1,1,{Self,1}}} =
        statistician:get_node_stats(Node1, raw),
    
    %...memory used changes while testing...
    {[Node1], [JobId1], 1.0, 1.0, 1, 1, 1, 1,_,_} =
        statistician:get_cluster_stats(raw),

    %Unless we remove the node from the stats.
    statistician:remove_node(Node1),
    
    {[], [],0.0,0.0,0,0,0,0,_,_} =
        statistician:get_cluster_stats(raw),
    {error, no_such_node_in_stats} =
        statistician:get_node_stats(Node1, raw),
    %for coverage
    statistician:job_finished(JobId1),
    Config.

