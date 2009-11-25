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
     job_finished
    ].

init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    ok = application:start(common),
    ok = application:start(chronicler),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(_Config) ->
    ok = application:stop(common),
    ok = application:stop(chronicler),
    ok.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(ets_exists, Config) ->
    {ok, _Pid} = statistician:start_link(master),
    Config;
init_per_testcase(mem_and_disk, Config) ->
    {ok, _Pid} = statistician:start_link(master),
    Config;
init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = statistician:start_link(master),
    MasterDisk = statistician:get_node_disk_usage(raw),
    MasterMem = statistician:get_node_mem_usage(raw),
    Node1 = node(),
    Node2 = 'fakenode@10.10.10.10',
    Node3 = 'notrealnode@20.20.20.20',
    Node4 = 'mongo@10.20.30.40',
    {Mega, Sec, Micro} = now(),
    JobId1 = list_to_integer(lists:concat([Mega, Sec, Micro])),
    JobId2 = JobId1 + 23124,
    JobId3 = JobId1 - 14155,
    [{Pid, MasterDisk, MasterMem,
     {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}
     | Config].

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(_TestCase, _Config) ->
    statistician:stop(),
    ok.


ets_exists(Config) ->
    true = ets:info(job_stats_table) =/= undefined,
    Config.

mem_and_disk(Config) ->
    {NMTotalSize, NMTotalPercentage, {_NMWorstPid, NMWorstSize}} =
        statistician:get_node_mem_usage(raw),
    {NDTotalSize, NDTotalPercentage} =
        statistician:get_node_disk_usage(raw),
    [{per_node, _}, {collected, [CMTotalSize, CMTotalUsed, CMAverageSize, CMTotalPercentage, CMAveragePercentage]}] =
        statistician:get_cluster_mem_usage(raw),
    [{per_node, _}, {collected, [CDTotalSize, CDTotalUsed, CDAverageSize, CDTotalPercentage, CDAveragePercentage]}] =
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

    %Can't (easily) check the output of strings, so we just make sure they
    %really are strings.
    true = is_list(statistician:get_node_mem_usage(string)),
    true = is_list(statistician:get_node_disk_usage(string)),
    true = is_list(statistician:get_cluster_mem_usage(string)),
    true = is_list(statistician:get_cluster_disk_usage(string)),

    Config.


checkempty([{_Pid, MasterDisk, MasterMem,
             {JobId1, _JobId2, _JobId3}, {Node1, _Node2, _Node3, _Node4}}
            | Config]) ->
    %Stats tables should be empty.
    {[], [],0.0,0.0,0,0,0,0,MasterDisk,MasterMem} =
        statistician:get_cluster_stats(raw),
    {error, no_such_stats_found} =
        statistician:get_job_stats(JobId1, string),
    {error, no_such_node_in_stats} =
        statistician:get_node_stats(Node1, raw),
    {error, no_such_stats_found} =
        statistician:get_node_job_stats(Node1, JobId1, string),
    Config.

update_and_getters([{_Pid, MasterDisk, MasterMem,
                     {JobId1, JobId2, JobId3}, {Node1, Node2, Node3, Node4}}
                    | Config]) ->

    %Lets add some fake info!
    statistician:update({{Node1, JobId1, split}, 0.0, 0.0, 0, 0, 0, 0,
                         {0,0},{0,0,{self(),0}}}),
    statistician:update({{Node1, JobId3, split}, 1.0, 1.0, 1, 1, 1, 1,
                         {1,1},{1,1,{self(),1}}}),
    statistician:update({{Node2, JobId2, map}, 2.0, 2.0, 2, 2, 2, 2,
                         {2,2},{2,2,{self(),2}}}),
    statistician:update({{Node3, JobId3, reduce}, 3.0, 3.0, 3, 3, 3, 3,
                         {3,3},{3,3,{self(),3}}}),
    statistician:update({{Node4, JobId1, finalize}, 2.0, 2.0, 2, 2, 2, 2,
                         {2,2},{2,2,{self(),2}}}),
    %Should update existing entry and add together, for a resulting
    %table entry of {{_,JobId1,finalize},4.0,4.0,4,4,4,{4,4},{4,4}}
    statistician:update({{Node4, JobId1, finalize}, 2.0, 2.0, 2, 2, 2, 2,
                         {4,4},{4,4,{self(),4}}}),

    %Check if the update went as expected
    [4.0, 4.0, 4, 4, 4, 4] =
        statistician:get_job_stats(JobId1, raw),
    %Can't check string results automatically...
    true = [4.0, 4.0, 4, 4, 4, 4] =/=
        statistician:get_job_stats(JobId1, string),
    [0.0, 0.0, 0, 0, 0, 0] =
        statistician:get_node_job_stats(Node1, JobId1, raw),
    true = [0.0, 0.0, 0, 0, 0, 0] =/=
        statistician:get_node_job_stats(Node1, JobId1, string),

    Self = self(),
    {{Node1}, [JobId3, JobId1], 1.0,1.0,1,1,1,1,{1,1},{1,1,{Self,1}}} =
        statistician:get_node_stats(Node1, raw),
    true = {{Node1}, [JobId3, JobId1], 1.0,1.0,1,1,1,1,{1,1},{1,1,{Self,1}}} =/=
        statistician:get_node_stats(Node1, string),
    {Nodes, JobIds, 10.0,10.0,10,10,10,10,MasterDisk,MasterMem} =
        statistician:get_cluster_stats(raw),
    [] = JobIds -- [JobId1, JobId2, JobId3] ++ Nodes -- [Node1, Node2, Node3, Node4],
    true = is_list(statistician:get_cluster_stats(string)),
    Config.

job_finished([{_Pid, MasterDisk, MasterMem,
               {JobId1, _JobId2, _JobId3}, {Node1, _Node2, _Node3, _Node4}}
              | Config]) ->

    %Lets add some fake info!
    statistician:update({{Node1, JobId1, split}, 1.0, 1.0, 1, 1, 1, 1,
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
    {[Node1], [JobId1], 1.0, 1.0, 1, 1, 1, 1,MasterDisk,MasterMem} =
        statistician:get_cluster_stats(raw),
    Config.
