%%%-------------------------------------------------------------------
%%% @author Axel "Align" Andren <axelandren@gmail.com>
%%% @author Bjorn "norno" Dahlman <bjorn.dahlman@gmail.com>
%%% @author Gustav "azariah" Simonsson <gusi7871@student.uu.se>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Collects various statistics about the cluster and its nodes, like
%%% power consumption and time taken for jobs to complete.
%%%
%%% @end
%%% Created : 21 Oct 2009 by Axel Andren <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(statistician).
-include("../include/env.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, update/1, job_finished/1, remove_node/1, stop/0,
         get_cluster_stats/0, get_job_stats/1,
         get_node_stats/1, get_node_job_stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(UPDATE_INTERVAL, 1000).

-ifndef(debug).
-define(DELETE_TABLE(), delete).
-else.
-define(DELETE_TABLE(), dont).
-endif.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% If Type is the atom master, we create a global stats table as well
%% as the local.
%% If it's the atom slave, we only have the local table, but start
%% an interval timer to send it to master regularly.
%% 
%% Type = master | slave
%% @spec start_link(Type) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Type) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Type], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the statistician.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).


%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string of stats for entire cluster.
%%
%% @spec get_cluster_stats() -> String
%% @end
%%--------------------------------------------------------------------
get_cluster_stats() ->
    gen_server:call(?MODULE,{get_cluster_stats}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string of all the stats related to JobId
%%
%% @spec get_job_stats(JobId) -> String
%% @end
%%--------------------------------------------------------------------
get_job_stats(JobId) ->
    gen_server:call(?MODULE,{get_job_stats, JobId}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string of all the stats related to NodeId
%%
%% @spec get_node_stats(NodeId) -> String
%% @end
%%--------------------------------------------------------------------
get_node_stats(NodeId) ->
    gen_server:call(?MODULE,{get_node_stats, NodeId}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string of stats the node NodeId has on the job
%% JobId, like how many JobId tasks NodeId has worked on, or how long.
%%
%% @spec get_node_job_stats(NodeId, JobId) -> String
%% @end
%%--------------------------------------------------------------------
get_node_job_stats(NodeId, JobId) ->
    gen_server:call(?MODULE,{get_node_job_stats, NodeId, JobId}).


%%--------------------------------------------------------------------
%% @doc
%% Updates local (node) ets table with statistics, adding the job and
%% its stats to the table if it doesn't already exist, otherwise
%% updating the existing entry.
%%
%% @spec update(Data) -> ok
%% @end
%%--------------------------------------------------------------------
update(Data) ->
    gen_server:cast(?MODULE,{update, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Jobs that are finished in our cluster should have their stats dumped to 
%% file and their entry cleared out of the table, but we have to wait
%% to make sure that all slaves have sent their stats updates.  This is
%% by no means a sure thing, but we hope that waiting two update intervals
%% will be sufficient.
%% Due to sending a message to itself after the waiting time has passed,
%% we must use handle_info, though it passes the call onto handle_cast.
%%
%% @spec job_finished(JobId) -> please_wait_a_few_seconds
%% @end
%%--------------------------------------------------------------------
%%TODO look at this
job_finished(JobId) ->
    {ok, _TimerRef} = timer:send_after(?UPDATE_INTERVAL*2, ?MODULE,
                                       {job_finished, JobId}),
    please_wait_a_few_seconds.

%%--------------------------------------------------------------------
%% @doc
%% If a node dies, we should remove it from our (global) stats.
%% It should be safe to do so immediately, since it has left the cluster.
%% 
%% @spec remove_node(NodeId) -> ok
%% @end
%%--------------------------------------------------------------------
remove_node(NodeId) ->
    gen_server:cast(?MODULE, {remove_node, NodeId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server, call with Args as [master] to start master,
%% [slave] to start slave. See start_link.
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([master]) ->
    global:register_name(?MODULE, self()),
    ets:new(job_stats_table,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    ets:new(node_stats_table,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    ets:new(cluster_stats_table,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    {ok, []};
init([slave]) ->
    {ok, _TimerRef} = timer:send_interval(?UPDATE_INTERVAL, flush),
    ets:new(job_stats_table,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see node_stats/1
%%
%% @spec handle_call({get_cluster_stats}, From, State) ->
%%                          {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_cluster_stats}, _From, State) ->
    Reply = cluster_stats(),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see node_job_stats/1
%%
%% @spec handle_call({get_job_stats, JobId}, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_job_stats, JobId}, _From, State) ->
    Reply = node_job_stats('_', JobId),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see node_stats/1
%%
%% @spec handle_call({get_node_stats, NodeId}, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_stats, NodeId}, _From, State) ->
    Reply = node_stats(NodeId),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see node_job_stats/1
%%
%% @spec handle_call({get_node_job_stats, NodeId, JobId}, From, State)
%%                                -> {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_job_stats, NodeId, JobId}, _From, State) ->
    Reply = node_job_stats(NodeId, JobId),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:warning("~w:Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see update/1
%%
%% @spec handle_cast({update, StatsTuple}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({update,
             {{NodeId, JobId, TaskType},
              Power, Time, Upload, Download, Numtasks, Restarts}},
            State) ->
    T = job_stats_table,
    Job = ets:lookup(T, {NodeId, JobId, TaskType}),
    case Job of
        [] ->
            ets:insert(T, {{NodeId, JobId, TaskType},
                           Power, Time, Upload, Download, Numtasks, Restarts});
        
        [{{NodeId, JobId, TaskType},
          OldPower,OldTime, OldUpload,OldDownload, OldNumtasks,OldRestarts}] ->
            
            ets:insert(T, {{NodeId, JobId, TaskType},
                           Power    + OldPower,
                           Time     + OldTime,
                           Upload   + OldUpload,
                           Download + OldDownload,
                           Numtasks + OldNumtasks,
                           Restarts + OldRestarts})
    end,
    T2 = node_stats_table,
    T3 = cluster_stats_table,
    case ets:info(T2) of
        undefined ->
            foo; %only master has node_stats_table defined
        _Other ->
            Item1 = ets:lookup(T2, {NodeId, JobId, TaskType}),
            case Item1 of
                [] ->
                    ets:insert(T2, {{NodeId, JobId, TaskType},
                                    Power, Time, Upload,
                                    Download, Numtasks, Restarts});
                
                [{{NodeId, JobId, TaskType},
                  OldNodePower, OldNodeTime, OldNodeUpload, OldNodeDownload,
                  OldNodeNumtasks, OldNodeRestarts}] ->
                    
                    ets:insert(T2, {{NodeId, JobId, TaskType},
                                    Power    + OldNodePower,
                                    Time     + OldNodeTime,
                                    Upload   + OldNodeUpload,
                                    Download + OldNodeDownload,
                                    Numtasks + OldNodeNumtasks,
                                    Restarts + OldNodeRestarts})
            end,
            Item2 = ets:lookup(T3, {NodeId, JobId, TaskType}),
            case Item2 of
                [] ->
                    ets:insert(T3, {{NodeId, JobId, TaskType},
                                    Power, Time, Upload,
                                    Download, Numtasks, Restarts});
                
                [{{NodeId, JobId, TaskType},
                  OldClusterPower, OldClusterTime,
                  OldClusterUpload, OldClusterDownload,
                  OldClusterNumtasks, OldClusterRestarts}] ->  
                    ets:insert(T3, {{NodeId, JobId, TaskType},
                                    Power    + OldClusterPower,
                                    Time     + OldClusterTime,
                                    Upload   + OldClusterUpload,
                                    Download + OldClusterDownload,
                                    Numtasks + OldClusterNumtasks,
                                    Restarts + OldClusterRestarts})
            end
    end,
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see stop/0
%%
%% @spec handle_cast(stop, State) -> {stop, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Splits the received list and updates the master table with each 
%% element in the lists.
%%
%% @spec handle_cast({update_with_list, List}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({update_with_list, List}, State) ->
    chronicler:debug("Master received message from a node.~n", []),
    lists:foreach(fun (X) -> gen_server:cast(?MODULE, {update, X}) end, List),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see job_finished/1
%%
%% @spec handle_cast({job_finished, JobId}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({job_finished, JobId}, State) ->
    JobStats = node_job_stats('_', JobId),
    case ?DELETE_TABLE() of
	delete ->
	    ets:match_delete(job_stats_table, {{'_', JobId, '_'},'_','_','_','_','_','_'});
	_Dont ->
	    ok
    end,
    {ok, Root} =
        configparser:read_config(?CONFIGFILE, cluster_root),
    file:write_file(Root ++ "results/" ++
                  integer_to_list(JobId) ++ "/stats", JobStats),
    chronicler:info(JobStats),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see remove_node/1
%% We do not delete data from the job stats tables when a node leaves,
%% only from the global stats. 
%% Should produce a file: /storage/test/results/node_#_stats
%%
%% @spec handle_cast({remove_node, NodeId}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({remove_node, NodeId}, State) ->
    NodeStats = node_stats(NodeId),
    ets:match_delete(node_stats_table, 
                     {{NodeId, '_', '_'},'_','_','_','_','_','_'}),
    {ok, Root} =
        configparser:read_config(?CONFIGFILE, cluster_root),
    file:write_file(Root ++ "results/node_" ++
                  atom_to_list(NodeId) ++ "_stats", NodeStats),
    chronicler:info(NodeStats),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w:Received unexpected handle_cast call.~n"
                       "Message: ~p~n",
                       [?MODULE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends contents of local stats table to the master stats table,
%% then clears out the local stats table.
%%
%% @spec handle_info(flush, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(flush, State) ->
    chronicler:debug("Node ~p flushing stats.~n", [node()]),
    StatsList = ets:tab2list(job_stats_table),
    gen_server:cast({global, ?MODULE}, {update_with_list, StatsList}),
    ets:delete_all_objects(job_stats_table),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see job_finished/1
%%
%% @spec handle_info({job_finished, JobId}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info({job_finished, JobId}, State) ->
    gen_server:cast(?MODULE, {job_finished, JobId}),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    chronicler:warning("~w:Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(normal, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) -> 
    chronicler:debug("~w:Received normal terminate call.~n"),
    ok;
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) -> 
    chronicler:debug("~w:Received terminate call.~n"
                     "Reason: ~p~n",
                     [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% Logs and discards unexpected messages.
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> 
    chronicler:warning("~w:Received unexpected code_change call.~n"
                       "Old version: ~p~n"
                       "Extra: ~p~n",
                       [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Extracts stats that NodeId has on JobId and returns a formatted
%% string showing these. get_job_stats/1 does not want stats on a
%% specific node, and so passes the atom '_' as NodeId, resulting
%% in a list of nodes that have worked on the job being matched out.
%%
%% @spec node_job_stats(NodeId, JobId) -> String
%% 
%% @end
%%--------------------------------------------------------------------
node_job_stats(NodeId, JobId) ->
    T = case NodeId of
            '_' -> job_stats_table;
            _Id -> node_stats_table
        end,
    %could go with the regular stats table, only then we'd be limited
    %to only being able to check the node_job stats while the job is
    %still in the system.
    case ets:match(T, {{NodeId, JobId, '_'}, '$1', '_', '_', '_', '_', '_'}) of
        [] ->
            {error, no_such_stats_found};
        _Other ->
            Split    = ets:match(T, {{NodeId, JobId, split},
                                     '$1', '$2', '$3', '$4', '$5', '$6'}),
            Map      = ets:match(T, {{NodeId, JobId, map},
                                     '$1', '$2', '$3', '$4', '$5', '$6'}),
            Reduce   = ets:match(T, {{NodeId, JobId, reduce},
                                     '$1', '$2', '$3', '$4', '$5', '$6'}),
            Finalize = ets:match(T, {{NodeId, JobId, finalize},
                                     '$1', '$2', '$3', '$4', '$5', '$6'}),
            Nodes = case NodeId of
                        '_' -> lists:umerge(
                                 ets:match(T, {{'$1', JobId, '_'},
                                               '_','_','_','_','_','_'}));
                        _NodeId -> NodeId
                    end,
            
            Zeroes = [0,0,0,0,0,0],
            
            SumSplit  = sum_stats(Split, Zeroes),
            SumMap    = sum_stats(Map, Zeroes),
            SumReduce = sum_stats(Reduce, Zeroes),
            SumFinal  = sum_stats(Finalize, Zeroes),
            
            SumAll = sum_stats([SumSplit, SumMap, SumReduce, SumFinal], Zeroes),

	    TimeList = integer_to_list(JobId),
	    Then = {list_to_integer(lists:sublist(TimeList, 4)),
		    list_to_integer(lists:sublist(TimeList, 5, 6)),
		    list_to_integer(lists:sublist(TimeList, 11, 6))},
	    TimePassed = timer:now_diff(now(), Then) / 1000000,
	    
            SplitStrings  = taskstats_string_formatter(split, SumSplit),
            MapStrings    = taskstats_string_formatter(map, SumMap),
            ReduceStrings = taskstats_string_formatter(reduce, SumReduce),
            FinalStrings  = taskstats_string_formatter(finalize, SumFinal),
            Reply = jobstats_string_formatter({JobId,
                                               SplitStrings,
                                               MapStrings,
                                               ReduceStrings,
                                               FinalStrings,
                                               TimePassed,
                                               Nodes,
                                               SumAll}),
            Reply
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extracts stats about NodeId and returns a formatted string showing these.
%%
%% @spec node_stats(NodeId) -> String
%% 
%% @end
%%--------------------------------------------------------------------
node_stats(NodeId) ->
    T = node_stats_table,
    case ets:match(T, {{NodeId, '_', '_'}, '$1', '_', '_', '_', '_', '_'}) of
        [] ->
            {error, no_such_node_in_stats};
        _Other ->
            Jobs = ets:match(T, {{NodeId, '$1','_'},'_','_','_','_','_','_'}),
            UniqueJobs = lists:umerge(Jobs),

            Stats = ets:match(T, {{NodeId,'_','_'},
                                 '$1','$2','$3','$4','$5','$6'}),
            [Power, Time, Upload, Download, Numtasks, Restarts] =
                sum_stats(Stats, [0,0,0,0,0,0]),

            
            Reply = nodestats_string_formatter({NodeId, UniqueJobs,
                                                Power, Time,
                                                Upload, Download,
                                                Numtasks, Restarts}),
            Reply
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extracts stats about the entire cluster, and returns a formatted
%% string showing these.
%%
%% @spec cluster_stats() -> String
%% 
%% @end
%%--------------------------------------------------------------------
cluster_stats() ->
    CollectStuff =
        fun ({{Node, Job, _}, Power, Time, Upload, Download, NumTasks, Restarts},
             {Nodes, Jobs, PowerAcc, TimeAcc, UpAcc, DownAcc, TasksAcc, RestartsAcc}) ->
                  {[Node | Nodes], [Job | Jobs],
                  PowerAcc + Power,
                  TimeAcc + Time,
                  UpAcc + Upload,
                  DownAcc + Download,
                  TasksAcc + NumTasks,
                  RestartsAcc + Restarts}
        end,
    {Nodes, Jobs, Power, Time, Upload, Download, NumTasks, Restarts} =
        ets:foldl(CollectStuff, {[], [], 0, 0, 0, 0, 0, 0}, cluster_stats_table),
    UniqueNodes = lists:usort(Nodes),
    UniqueJobs  = lists:usort(Jobs),

    clusterstats_string_formatter({UniqueNodes, UniqueJobs,
                                   Power, Time,
                                   Upload, Download,
                                   NumTasks, Restarts}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for stats of the entire cluster.
%% Except that of nodes that have been removed from the cluster in any
%% manner.
%%
%% @spec clusterstats_string_formatter(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
clusterstats_string_formatter(
  {Nodes, Jobs, Power, Time, Upload, Download, Numtasks, Restarts}) ->
    lists:flatten(
      io_lib:format(
        "The cluster currently has these stats stored:~n"
        "------------------------------------------------------------~n"
        "Nodes used: ~p~n"
        "Jobs worked on: ~p~n"
        "Time executing: ~.2f seconds~n"
        "Power used: ~.2f watt hours~n"
        "Upload: ~p bytes~n"
	    "Download: ~p bytes~n"
        "Number of tasks total: ~p~n"
        "Number of task restarts:~p~n",
        [Nodes, Jobs, Power / 3600, Time, Upload, Download, Numtasks, Restarts])).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given job and its stats
%%
%% @spec nodestats_string_formatter(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
nodestats_string_formatter(
  {NodeId, Jobs, Power, Time, Upload, Download, Numtasks, Restarts}) ->
    lists:flatten(
      io_lib:format(
        "Stats for node: ~p~n"
        "------------------------------------------------------------~n"
        "Jobs worked on by node: ~p~n"
        "Time executing: ~.2f seconds~n"
        "Power used: ~.2f watt hours~n"
        "Upload: ~p bytes~n"
	"Download: ~p bytes~n"
        "Number of tasks: ~p~n"
        "Number of task restarts:~p~n",
        [NodeId, Jobs, Power / 3600, Time, Upload, Download, Numtasks, Restarts])).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given job and its stats
%%
%% @spec jobstats_string_formatter(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
jobstats_string_formatter(
  {JobId, SplitString, MapString, ReduceString, FinalizeString, TimePassed,
   Nodes, [Power, TimeExecuted, Upload, Download, Numtasks, Restarts]}) ->
    lists:flatten(
      io_lib:format(
        "Stats for job: ~p~n~ts~ts~ts~ts~n"
        "------------------------------------------------------------~n"
        "Total:~n"
        "------------------------------------------------------------~n"
        "Nodes that worked on job: ~p~n"
        "Time passed: ~.2f seconds~n"
        "Execution time: ~.2f seconds~n"
        "Power used: ~.2f watt hours~n"
        "Upload: ~p bytes~n"
	"Download: ~p bytes~n"
        "Number of tasks: ~p~n"
        "Number of restarts: ~p~n",
        [JobId, SplitString, MapString, ReduceString, FinalizeString, Nodes,
	 TimePassed,TimeExecuted, Power / 3600, Upload,Download, Numtasks, Restarts])).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given task and its stats
%%
%% @spec taskstats_string_formatter(TaskType, TaskStats) -> String
%% 
%% @end
%%--------------------------------------------------------------------
taskstats_string_formatter(TaskType,
                           [Power,Time,Upload,Download,NumTasks,Restarts]) ->
    io_lib:format(
      "------------------------------------------------------------~n"
      "~p~n"
      "------------------------------------------------------------~n"
      "Power used: ~.2f watt seconds~n"
      "Time spent: ~.2f seconds~n"
      "Upload: ~p bytes~n"
      "Download: ~p bytes~n"
      "Number of tasks: ~p~n"
      "Number of restarts: ~p~n",
      [TaskType, Power, Time, Upload, Download, NumTasks, Restarts]).


%%--------------------------------------------------------------------
%% @doc
%% Every element in the given stats list is summed up.
%%
%% @spec sum_stats(List, Data) -> Data + List
%% 
%% @end
%%--------------------------------------------------------------------
sum_stats([],Data) ->
    Data;
sum_stats([H|T], Data) ->
    [TempPower,TempTime,TempUpload,TempDownload,TempNumtasks,TempRestarts]  = H,
    [AccPower,AccTime,AccUpload,AccDownload,AccNumtasks,AccRestarts] = Data,
    sum_stats(T, [TempPower    + AccPower,
                  TempTime     + AccTime,
                  TempUpload   + AccUpload,
		          TempDownload + AccDownload,
                  TempNumtasks + AccNumtasks,
                  TempRestarts + AccRestarts]).
    
