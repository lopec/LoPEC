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

-behaviour(gen_server).

%% API
-export([start_link/1, update/1, job_finished/1, get_job_stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(UPDATE_INTERVAL, 1000).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% If Type is the atom master, we create a global stats table as well
%% as the local.
%% If it's the atom slave, we only have the local table, but start
%% an interval timer to flush it to master intermittently.
%%
%% @spec start_link(Type) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Type) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [Type], []).

%%--------------------------------------------------------------------
%% @doc
%% Returns a formatted string of all the stats related to JobID
%%
%% @spec get_job_stats(JobID) -> String
%% @end
%%--------------------------------------------------------------------
get_job_stats(JobID) ->
    gen_server:call(?SERVER,{get_job_stats, JobID}).



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
    gen_server:cast(?SERVER,{update, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Jobs that are finished in our cluster should be dumped to file and
%% cleared out of our stats table, but we have to wait to make sure
%% that all slaves have sent their stat updates. This is NOT exact,
%% but the hope is that waiting two update intervals will be plenty
%% of time.
%%
%% @spec job_finished(JobID) -> please_wait_a_few_seconds
%% @end
%%--------------------------------------------------------------------
job_finished(JobID) ->
    {ok, _TimerRef} = timer:send_after(?UPDATE_INTERVAL*2, ?SERVER,
                                       {job_finished, JobID}),
    please_wait_a_few_seconds.

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
    global:register_name(?SERVER, self()),
    ets:new(stats,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    ets:new(global_stats_master,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    {ok, []};
init([slave]) ->
    {ok, _TimerRef} = timer:send_interval(?UPDATE_INTERVAL, flush),
    ets:new(stats,
            [set, private, named_table,
             {keypos, 1}, {heir, none},
             {write_concurrency, false}]),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see job_stats/1
%%
%% @spec handle_call({get_jobs_stats, JobID}, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({get_job_stats, JobID}, _From, State) ->
    Reply = job_stats(JobID),
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
             {{NodeName, JobID, TaskType},
              Power, Time, Netload, Numtasks, Restarts}},
            State) ->
    
    Item = ets:lookup(stats, {NodeName, JobID, TaskType}),
    case Item of
        [] ->
            ets:insert(stats, {{NodeName, JobID, TaskType},
                        Power, Time, Netload, Numtasks, Restarts});
        [{{_,_,_}, OldPower, OldTime, OldNetload, OldNumtasks, OldRestarts}] ->
            ets:insert(stats, {{NodeName, JobID, TaskType},
                        Power+OldPower,
                         Time+OldTime,
                         Netload+OldNetload,
                         Numtasks+OldNumtasks,
                         Restarts+OldRestarts})
    end,
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Breaks the received list into small bits and updates the master table
%% with the contents.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({update_with_list, List}, State) ->
    lists:foreach(fun (X) -> gen_server:cast(?SERVER, {update, X}) end, List),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see job_finished/1
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({job_finished, JobID}, State) ->
    JobStats = job_stats(JobID),
    ets:match_delete(stats, {{'_', JobID, '_'},'_','_','_','_','_'}),
    {ok, Root} =
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    file:write_file(Root ++ "results/" ++
                  integer_to_list(JobID) ++ "/stats.txt", JobStats),
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
    Stats = ets:tab2list(stats),
    gen_server:cast({global, ?SERVER}, {update_with_list, Stats}),
    ets:delete_all_objects(stats),
    {noreply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @see job_finished/1
%%
%% @spec handle_info({job_finished, JobID}, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info({job_finished, JobID}, State) ->
    gen_server:cast(?SERVER, {job_finished, JobID}),
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
%% Extracts info about the given job and returns a formatted string
%% with its stats.
%%
%% @spec job_stats(JobID) -> String
%% 
%% @end
%%--------------------------------------------------------------------
job_stats(JobID) ->
    T = stats,
    case ets:match(T, {{'_', JobID, '_'}, '$1', '_', '_', '_', '_'}) of
        [] ->
            {error, no_such_job_in_stats};
        _Other ->
            Split    = ets:match(T, {{'_', JobID, split}, '$1', '$2', '$3', '$4', '$5'}),
            Map      = ets:match(T, {{'_', JobID, map}, '$1', '$2', '$3', '$4', '$5'}),
            Reduce   = ets:match(T, {{'_', JobID, reduce}, '$1', '$2', '$3', '$4', '$5'}),
            Finalize = ets:match(T, {{'_', JobID, finalize}, '$1', '$2', '$3', '$4', '$5'}),
            Nodes    = ets:match(T, {{'$1', JobID, '_'},'_','_','_','_','_'}),
            UniqueNodes = lists:umerge(Nodes),
            
            Zeroes = [0,0,0,0,0],
            SumSplit = sum_stats(Split, Zeroes),
            SumMap = sum_stats(Map, Zeroes),
            SumReduce = sum_stats(Reduce, Zeroes),
            SumFinal = sum_stats(Finalize, Zeroes),
            
            SumAll = sum_stats([SumSplit, SumMap, SumReduce, SumFinal], Zeroes),

            {Mega, Sec, Milli} = now(),

             TimePassed = ((list_to_integer(
                              integer_to_list(Mega) ++
                              integer_to_list(Sec) ++
                              integer_to_list(Milli))) - JobID) / 1000,
            
            SplitStrings = taskstats_string_formatter(split, SumSplit),
            MapStrings = taskstats_string_formatter(map, SumMap),
            ReduceStrings = taskstats_string_formatter(reduce, SumReduce),
            FinalStrings = taskstats_string_formatter(finalize, SumFinal),
            Reply = jobstats_string_formatter({JobID,
                                               SplitStrings,
                                               MapStrings,
                                               ReduceStrings,
                                               FinalStrings,
                                               TimePassed,
                                               UniqueNodes,
                                               SumAll}),
            Reply
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given job and its stats
%%
%% @spec jobstats_string_formatter(Data) -> String
%% 
%% @end
%%--------------------------------------------------------------------
jobstats_string_formatter(
  {JobID, SplitString, MapString, ReduceString, FinalizeString, TimePassed,
   Nodes, [TimeExecuted, Power, Netload, Numtasks, Restarts]}) ->
    lists:flatten(
      io_lib:format(
        "Stats for job: ~p~n~ts~ts~ts~ts~n"
        "------------------------------------------------------------~n"
        "Total:~n"
        "------------------------------------------------------------~n"
        "Nodes that worked on job: ~p~n"
        "Time passed: ~p~n"
        "Execution time: ~p~n"
        "Power used: ~p~n"
        "Network traffic: ~p~n"
        "Number of tasks: ~p~n"
        "Number of restarts:~p~n",
        [JobID, SplitString, MapString, ReduceString, FinalizeString,
         Nodes, TimePassed, TimeExecuted, Power, Netload, Numtasks, Restarts])).

%%--------------------------------------------------------------------
%% @doc
%% Returns a neatly formatted string for the given task and its stats
%%
%% @spec taskstats_string_formatter(TaskType, TaskStats) -> String
%% 
%% @end
%%--------------------------------------------------------------------
taskstats_string_formatter(TaskType, [Power, Time, NetLoad, NumTasks, Restarts]) ->
    io_lib:format(
      "------------------------------------------------------------~n"
      "~p~n"
      "------------------------------------------------------------~n"
      "Power used: ~p~n"
      "Time spent: ~p~n"
      "Network traffic: ~p~n"
      "Number of tasks: ~p~n"
      "Number of restarts: ~p~n",
      [TaskType, Power, Time, NetLoad, NumTasks, Restarts]).


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
    [TempPower, TempTime, TempNetload, TempNumtasks, TempRestarts]  = H,
    [AccPower, AccTime, AccNetload, AccNumtasks, AccRestarts] = Data,
    sum_stats(T, [TempPower + AccPower,
                  TempTime + AccTime,
                  TempNetload + AccNetload,
                  TempNumtasks + AccNumtasks,
                  TempRestarts + AccRestarts]).
    
