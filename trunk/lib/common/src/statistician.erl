%%%-------------------------------------------------------------------
%%% @author Axel "Align" Andren <axelandren@gmail.com>
%%% @author Bjorn "norno" Dahlman <bjorn.dahlman@gmail.com>
%%% @author Gustav "azariah" Simonsson <gusi7871@student.uu.se>
%%% @author Vasilij "Chabbrik" Savin <vasilij.savin@gmail.com>
%%% @copyright (C) 2009, Axel Andren <axelandren@gmail.com>
%%% @doc
%%%
%%% Collects various statistics about the cluster and its nodes, like
%%% power consumption and time taken for jobs to complete, and lets
%%% users access these through the API.
%%%
%%% @end
%%% Created : 21 Oct 2009 by Axel Andren <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(statistician).
-include("../include/env.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, update/1, job_finished/1, remove_node/1, stop/0,
         get_cluster_stats/1, get_job_stats/2,
         get_node_stats/2, get_node_job_stats/3,
	 get_node_disk_usage/1, get_node_mem_usage/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Milliseconds
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
%% <pre>
%% Type:
%%  slave - start an interval timer to flush gathered stats to master regularly
%%  master - no timer, but create a second ets table for storing node stats in
%% </pre>
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
%% Returns disk usage on a node.
%% <pre>
%% Flag:
%%  raw - gives internal representation (Tuples, lists, whatnot)
%%  string - gives nicely formatted string
%% </pre>
%% @spec get_node_disk_usage(Flag) -> String() | {Total::Integer(),
%% Percentage::Integer()}
%% @end
%%--------------------------------------------------------------------
get_node_disk_usage(raw) ->
    gen_server:call(?MODULE,{get_node_disk_usage, raw});
get_node_disk_usage(string) ->
    Return = gen_server:call(?MODULE,{get_node_disk_usage, string}),
    io:format(Return).

%%--------------------------------------------------------------------
%% @doc
%% Returns memory usage on a node.
%% <pre>
%% Flag:
%%  raw - gives internal representation (Tuples, lists, whatnot)
%%  string - gives nicely formatted string
%% </pre>
%% @spec get_node_disk_usage(Flag) -> String() | {Total::Integer(),
%% Percentage::Integer()}
%% @end
%%--------------------------------------------------------------------

get_node_mem_usage(raw) ->
    gen_server:call(?MODULE,{get_node_mem_usage, raw});
get_node_mem_usage(string) ->
    Return = gen_server:call(?MODULE,{get_node_mem_usage, string}),
    io:format(Return).
    
%%--------------------------------------------------------------------
%% @doc
%% Returns stats for the entire cluster.
%% <pre>
%% Flag:
%%  raw - gives internal representation (Tuples, lists, whatnot)
%%  string - gives nicely formatted string
%% </pre>
%% @spec get_cluster_stats(Flag) -> String
%% @end
%%--------------------------------------------------------------------
get_cluster_stats(raw) ->
    gen_server:call(?MODULE,{get_cluster_stats, raw});
get_cluster_stats(string) ->
    Return = gen_server:call(?MODULE,{get_cluster_stats, string}),
    io:format(Return).

%%--------------------------------------------------------------------
%% @doc
%% Returns stats for JobId.
%% <pre>
%% Flag:
%%  raw - gives internal representation (a list of the total stats)
%%  string - gives nicely formatted string with stats for each tasktype
%% </pre>
%% @spec get_job_stats(JobId, Flag) -> String
%% @end
%%--------------------------------------------------------------------
get_job_stats(JobId, raw) ->
    gen_server:call(?MODULE, {get_job_stats, JobId, raw});
get_job_stats(JobId, string) ->
    Return = gen_server:call(?MODULE,{get_job_stats, JobId, string}),
    case Return of
	{error, no_such_stats_found} ->
	    {error, no_such_stats_found};
	_Result ->
	    io:format(Return)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns stats for NodeId.
%% <pre>
%% Current statistics include: 
%%    * Jobs being executed
%%    * Power consumed
%%    * Time spent executing tasks
%%    * Upload network traffic
%%    * Download network traffic
%%    * Number of tasks processed
%%    * Number of task restarts
%%
%% Flag: 
%%      raw - gives internal representation (Tuples, lists, whatnot)
%%      string - gives nicely formatted string
%% </pre>
%% @spec get_node_stats(NodeId, Flag) -> String
%% @end
%%--------------------------------------------------------------------
get_node_stats(NodeId, raw) ->
    gen_server:call(?MODULE,{get_node_stats, NodeId, raw});
get_node_stats(NodeId, string) ->
    Return = gen_server:call(?MODULE,{get_node_stats, NodeId, string}),
    case Return of
	{error, no_such_node_in_stats} ->
	    {error, no_such_node_in_stats};
	_Result ->
	    io:format(Return)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns stats the node NodeId has for the job JobId, like how many
%% JobId tasks NodeId has worked on, or how long.
%% <pre>
%% Flag:
%%      raw - gives internal representation (Tuples, lists, whatnot)
%%      string - gives nicely formatted string
%% </pre>
%% @spec get_node_job_stats(NodeId, JobId, Flag) -> String
%% @end
%%--------------------------------------------------------------------
get_node_job_stats(NodeId, JobId, raw) ->
    gen_server:call(?MODULE,{get_node_job_stats, NodeId, JobId, raw});
get_node_job_stats(NodeId, JobId, string) ->
    Return=gen_server:call(?MODULE,{get_node_job_stats, NodeId, JobId, string}),
    case Return of
	{error, no_such_stats_found} ->
	    {error, no_such_stats_found};
	_Result ->
	    io:format(Return)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates local (node) ets table with statistics, adding the job and
%% its stats to the table if it doesn't already exist, otherwise
%% updating the existing entry.
%% <pre>
%% The Data variable should look like this tuple:
%% {{NodeId, JobId, TaskType},
%%      Power, Time, Upload, Download, NumTasks, Restarts}</pre>
%% 
%% @spec update(Data) -> ok
%% @end
%%--------------------------------------------------------------------
update(Data) ->
    gen_server:cast(?MODULE,{update, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Jobs, once finished in our cluster, have their stats dumped to file
%% and their entry cleared out of the ets table. However, we have to 
%% wait to make sure that all slaves have sent their stats updates -
%% we hope that waiting two update intervals will be sufficient, but
%% if a node is stalled for more than that long, we're out of luck.
%%
%% This wait is done using timer:send_after/3, which sends a regular
%% Erlang message, meaning we have to use handle_info/2 to catch
%% it. After the message is catched we pass the command onto
%% handle_cast/2 though.
%%
%% @spec job_finished(JobId) -> please_wait_a_few_seconds
%% @end
%%--------------------------------------------------------------------
job_finished(JobId) ->
    {ok, _TimerRef} = timer:send_after(?UPDATE_INTERVAL*2, ?MODULE,
                                       {job_finished, JobId}),
    please_wait_a_few_seconds.

%%--------------------------------------------------------------------
%% @doc
%% Remove a node from the global stats. Probably called when a node
%% drops from the cluster for some reason.
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
    {ok, []};
init([slave]) ->
    {ok, _TimerRef} = timer:send_interval(?UPDATE_INTERVAL, flush),
    ets:new(job_stats_table,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),

    % Setting up disk/mem alarm handler
    case os:cmd("uname") -- "\n" of
        "Darwin" ->
	    ok;
	"Linux" ->
	    application:start(sasl),
	    application:start(os_mon),
    
	    diskMemHandler:start({olol,ololigen});
	_ ->
	    chronicler:debug("~w : statistican init called on unsupported OS~n"),
	    ok
    end,

    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see node_stats/1
%%
%% @spec handle_call({get_cluster_stats, Flag}, From, State) ->
%%                          {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_cluster_stats, Flag}, _From, State) ->
    Reply = gather_cluster_stats(Flag),
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see get_node_disk_usage/1
%%
%% @spec handle_call({get_node_disk_usage, Flag}, From, State) ->
%%                          {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_disk_usage, Flag}, _From, State) ->
    F = fun() ->
		case os:cmd("uname") -- "\n" of
		    "Darwin" ->
			chronicler:debug("~w : disk_usage on mac unsupported~n"),
			_Stats = {0,0};
		    "Linux" ->
			{_Dir, Total, Percentage} = hd(disksup:get_disk_data()),
			_Stats = {Total, Percentage};
		    _ ->
			chronicler:debug("~w : disk_usage call on unsupported OS~n"),
			_Stats = {0,0}
		end
	end,
    {Total, Percentage} = F(),

    case Flag of
	raw ->
	    Reply = {Total, Percentage};
	string ->
	    Reply = lists:concat(["~nTotal disk size (Kb): ", Total,
				  "~nPercentage used: ", Percentage, "%~n"])
    end,
      
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see get_node_mem_usage/1
%%
%% @spec handle_call({get_node_mem_usage, Flag}, From, State) ->
%%                          {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_mem_usage, Flag}, _From, State) ->
    F = fun() ->
		case os:cmd("uname") -- "\n" of
		    "Darwin" ->
			chronicler:debug("~w : mem_usage on mac unsupported~n"),
			_Stats = {0,0};
		    "Linux" ->
			{Total, Alloc, _Lol} = memsup:get_memory_data(),
			Percentage = trunc((Alloc / Total) * 100),
			_Stats = {Total, Percentage};
		    _ ->
			chronicler:debug("~w : mem_usage call on unsupported OS~n"),
			_Stats = {0,0}
		end
	end,
    {Total, Percentage} = F(),

    case Flag of
	raw ->
	    Reply = {Total, Percentage};
	string ->
	    Reply = lists:concat(["~nTotal memory size (Bytes): ", Total,
				  "~nPercentage used: ", Percentage, "%~n"])
    end,
      
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see node_job_stats/1
%%
%% @spec handle_call({get_job_stats, JobId, Flag}, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_job_stats, JobId, Flag}, _From, State) ->
    Reply = gather_node_job_stats('_', JobId, Flag),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see node_stats/1
%%
%% @spec handle_call({get_node_stats, NodeId, Flag}, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_stats, NodeId, Flag}, _From, State) ->
    Reply = gather_node_stats(NodeId, Flag),
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flag = raw | string
%% @see node_job_stats/1
%%
%% @spec handle_call({get_node_job_stats, NodeId, JobId, Flag}, From, State)
%%                                -> {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_job_stats, NodeId, JobId, Flag}, _From, State) ->
    Reply = gather_node_job_stats(NodeId, JobId, Flag),
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
handle_cast({update, Stats}, State) ->
    {{NodeId, JobId, TaskType}, 
     Power, Time, Upload, Download, NumTasks, Restarts} = Stats,

    case ets:lookup(job_stats_table, {NodeId, JobId, TaskType}) of
        [] ->
            ets:insert(job_stats_table, Stats);
        [OldStats] ->
            {{_,_,_}, OldPower, OldTime, OldUpload,
             OldDownload, OldNumTasks, OldRestarts} = OldStats,
            
            ets:insert(job_stats_table, {{NodeId, JobId, TaskType},
                                         Power    + OldPower,
                                         Time     + OldTime,
                                         Upload   + OldUpload,
                                         Download + OldDownload,
                                         NumTasks + OldNumTasks,
                                         Restarts + OldRestarts})
    end,
    
    case ets:info(node_stats_table) of
        undefined ->
            %only master has node_stats_table defined
            table_undefined; 
        _Other ->
            case ets:lookup(node_stats_table, {NodeId}) of
                [] ->
                    ets:insert(node_stats_table, {{NodeId}, 
                                                  [JobId], Power, Time,
                                                  Upload, Download,
                                                  NumTasks, Restarts});
                [OldNodeStats] ->
                    {{_}, OldNodeJobs, OldNodePower, OldNodeTime, OldNodeUpload,
                     OldNodeDownload, OldNodeNumTasks, OldNodeRestarts}
                        = OldNodeStats,
                    
                    ets:insert(node_stats_table, {{NodeId},
                                                  lists:umerge([JobId], OldNodeJobs),
                                                  Power    + OldNodePower,
                                                  Time     + OldNodeTime,
                                                  Upload   + OldNodeUpload,
                                                  Download + OldNodeDownload,
                                                  NumTasks + OldNodeNumTasks,
                                                  Restarts + OldNodeRestarts})
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
    JobStats = gather_node_job_stats('_', JobId, string),
    case ?DELETE_TABLE() of
	delete ->
	    ets:match_delete(job_stats_table,
                             {{'_', JobId, '_'},'_','_','_','_','_','_'});
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
    NodeStats = gather_node_stats(NodeId, string),
    ets:match_delete(node_stats_table, 
                     {{NodeId},'_','_','_','_','_','_','_'}),
    {ok, Root} =
        configparser:read_config(?CONFIGFILE, cluster_root),
    file:write_file(Root ++ "results/node_" ++
                  atom_to_list(NodeId) ++ "_stats", NodeStats),
    chronicler:info("Node "++atom_to_list(NodeId)
                    ++" disconnected from cluster! Stats:~n" 
                    ++NodeStats),
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
    chronicler:debug("Node ~p transmitting stats.~n", [node()]),
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

%%--------------------------------------------------------------------
%% @doc
%% Returns the time (in microseconds) since the given job was added to
%% the cluster - sort of. It's derived from the JobId, which is in turn
%% based on now() being called when a job is created. So it's not
%% perfectly exact, but should be good enough for human purposes.
%%
%% @spec time_since_job_added(JobId) -> integer
%% 
%% @end
%%--------------------------------------------------------------------
time_since_job_added(JobId) ->
    TimeList = integer_to_list(JobId),
    Then = {list_to_integer(lists:sublist(TimeList, 4)),
            list_to_integer(lists:sublist(TimeList, 5, 6)),
            list_to_integer(lists:sublist(TimeList, 11, 6))},
    timer:now_diff(now(), Then) / 1000000.

%%--------------------------------------------------------------------
%% @doc
%% Extracts stats that NodeId has on JobId and returns a formatted
%% string showing these. get_job_stats/1 does not want stats on a
%% specific node, and so passes the atom '_' as NodeId, resulting
%% in a list of nodes that have worked on the job being matched out.
%% Flag = raw | string
%%
%% @spec gather_node_job_stats(NodeId, JobId, Flag) -> String
%% 
%% @end
%%--------------------------------------------------------------------
gather_node_job_stats(NodeId, JobId, Flag) ->
    T = job_stats_table,
    % TODO: ets:match is a potential bottleneck
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
            
            Zeroes = [0.0,0.0,0,0,0,0],
            
            SumSplit  = sum_stats(Split, Zeroes),
            SumMap    = sum_stats(Map, Zeroes),
            SumReduce = sum_stats(Reduce, Zeroes),
            SumFinal  = sum_stats(Finalize, Zeroes),
            SumAll = sum_stats([SumSplit, SumMap, SumReduce, SumFinal], Zeroes),

            case Flag of
                string ->
                    TimePassed = time_since_job_added(JobId),
                    
                    SplitStrings  = format_task_stats(split, SumSplit),
                    MapStrings    = format_task_stats(map, SumMap),
                    ReduceStrings = format_task_stats(reduce, SumReduce),
                    FinalStrings  = format_task_stats(finalize, SumFinal),
                    
                    format_job_stats({JobId, SplitStrings, MapStrings,
                                      ReduceStrings, FinalStrings, 
                                      TimePassed, Nodes, SumAll});
                raw ->
                    SumAll
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extracts statistics about Node and returns it as a formatted string.
%% Flag = raw | string
%%
%% @spec gather_node_stats(NodeId, Flag) -> String
%% 
%% @end
%%--------------------------------------------------------------------
gather_node_stats(NodeId, Flag) ->
    T = node_stats_table,
    % TODO: ets:match is a potential bottleneck
    case ets:lookup(T, {NodeId}) of
        [] ->
            {error, no_such_node_in_stats};
        [NodeStats] ->
            case Flag of
                raw ->
                    NodeStats;
                string ->
                    format_node_stats(NodeStats)
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extracts statistics about the entire cluster and returns it as a
%% formatted string.
%% Flag = raw | string
%%
%% @spec gather_cluster_stats(Flag) -> String
%% 
%% @end
%%--------------------------------------------------------------------
gather_cluster_stats(Flag) ->
    CollectStuff =
        fun ({{Node}, Jobs, Power, Time, Upload, Download, NumTasks, Restarts},
             {Nodes, JobsAcc, PowerAcc, TimeAcc,
	      UpAcc, DownAcc, TasksAcc, RestartsAcc}) ->
                  {[Node | Nodes], Jobs ++ JobsAcc,
                  PowerAcc + Power,
                  TimeAcc + Time,
                  UpAcc + Upload,
                  DownAcc + Download,
                  TasksAcc + NumTasks,
                  RestartsAcc + Restarts}
        end,
    
    {Nodes, Jobs, Power, Time, Upload, Download, NumTasks, Restarts} =
        ets:foldl(CollectStuff, {[], [], 0.0, 0.0, 0,0,0,0}, node_stats_table),
    Data = {lists:usort(Nodes), lists:usort(Jobs), 
             Power, Time, Upload, Download, NumTasks, Restarts},
    
    case Flag of
        raw ->
            Data;
        string ->
            format_cluster_stats(Data)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for stats of the entire cluster.
%%
%% @spec format_cluster_stats(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
format_cluster_stats(
  {Nodes, Jobs, Power, Time, Upload, Download, Numtasks, Restarts}) ->
      io_lib:format(
        "The cluster currently has these stats stored:~n"
        "------------------------------------------------------------~n"
        "Nodes used: ~p~n"
        "Jobs worked on: ~p~n"
        "Power used: ~.2f watt hours~n"
        "Time executing: ~.2f seconds~n"
        "Upload: ~p bytes~n"
        "Download: ~p bytes~n"
        "Number of tasks total: ~p~n"
        "Number of task restarts:~p~n",
        [Nodes, Jobs, Power / 3600, Time, Upload,
	 Download, Numtasks, Restarts]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string of the stats of the given node
%%
%% @spec format_node_stats(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
format_node_stats({{NodeId},
                  Jobs, Power, Time, Upload, Download, Numtasks, Restarts}) ->
    io_lib:format(
      "Stats for node: ~p~n"
      "------------------------------------------------------------~n"
      "Jobs worked on by node: ~p~n"
      "Power used: ~.2f watt hours~n"
      "Time executing: ~.2f seconds~n"
      "Upload: ~p bytes~n"
      "Download: ~p bytes~n"
      "Number of tasks: ~p~n"
      "Number of task restarts:~p~n",
      [NodeId, Jobs, Power / 3600, Time, Upload, Download, Numtasks, Restarts]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given job and its stats
%%
%% @spec format_job_stats(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
format_job_stats(
  {JobId, SplitString, MapString, ReduceString, FinalizeString, TimePassed,
   Nodes, [Power, TimeExecuted, Upload, Download, Numtasks, Restarts]}) ->
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
	 TimePassed,TimeExecuted, Power / 3600, Upload,
	 Download, Numtasks, Restarts]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given task and its stats
%%
%% @spec format_task_stats(TaskType, TaskStats) -> String
%% 
%% @end
%%--------------------------------------------------------------------
format_task_stats(TaskType, [Power,Time,Upload,Download,NumTasks,Restarts]) ->
    io_lib:format(
      "------------------------------------------------------------~n"
      "~p~n"
      "------------------------------------------------------------~n"
      "Power used: ~.2f watt seconds~n"
      "Execution time: ~.2f seconds~n"
      "Upload: ~p bytes~n"
      "Download: ~p bytes~n"
      "Number of tasks: ~p~n"
      "Number of restarts: ~p~n",
      [TaskType, Power, Time, Upload, Download, NumTasks, Restarts]).
