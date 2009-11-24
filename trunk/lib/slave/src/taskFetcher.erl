%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <bjorn.dahlman@gmail.com>
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Bjorn Dahlman & Fredrik Andersson
%%% @doc
%%% The taskFetcher is responsible for fetching and adding tasks.
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(taskFetcher).
-behaviour(gen_server).

%% API
-export([start_link/0,
         task_done/1,
         error/1,
         new_task/3]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(DYNSUP, dynamicSupervisor).
-define(WORKER, computingProcess).
-define(TASK_FETCH_INTERVAL, 1000).

-record(state, {work_state = no_task, timer}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    chronicler:info("~w : module started", [?MODULE]),
    gen_server:start_link({global, node()}, ?MODULE, no_args, []).

%%--------------------------------------------------------------------
%% @doc
%% Queries the dispatcher to create a new task.
%%
%% @spec new_task(Data, Type, Path) -> Task | {error, Error}
%% @end
%%--------------------------------------------------------------------
new_task(Data, Type, Path) ->
    chronicler:info("~w : called new_task of type ~w", [?MODULE, Type]),
    gen_server:call({global, node()}, {request, new_task, Data, Type, Path}).

task_done(Data) ->
    gen_server:cast({global, node()}, Data).

error(Data) ->
    gen_server:cast({global, node()}, Data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(no_args) ->
    {ok, TimerRef} = timer:send_interval(?TASK_FETCH_INTERVAL, poll),
    {Upload, Download} = netMonitor:get_net_stats(),
    NetStats = {Upload, Download},
    {ok, {#state{timer = TimerRef}, NetStats}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({request, task}, _From, State) ->
    request_task(),
    {reply, State, State};
handle_call({request, new_task, Data, Type, Path}, _From, State) ->
    Reply = give_task(Data, Type, Path),
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
    chronicler:warning("~w : Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({_Pid, done, {JobId, TaskId, Time, TaskType, _Progname}},
	    {_LolTimer, {OldUp, OldDown}}) ->
    %% Report to statistician
    Diff = timer:now_diff(now(), Time) / 1000000,
    Power = power_check:get_watt_per_task(Diff),
    {NewUp, NewDown} = netMonitor:get_net_stats(),
    Disk = statistician:get_node_disk_usage(raw),
    Mem = statistician:get_node_mem_usage(raw),
    statistician:update({{node(), JobId, list_to_atom(TaskType)},
                         Power, Diff, NewUp - OldUp, NewDown - OldDown, 1, 0,
			 Disk, Mem}),
    
    %% Kill and remove computingProcess spec from dynamic supervisor
    supervisor:terminate_child(?DYNSUP, ?WORKER),
    supervisor:delete_child(?DYNSUP, ?WORKER),

    %% Report to master that task is done
    dispatcher:report_task_done(TaskId),

    %% Reinstate poll timer and request task
    {ok, Timer} = timer:send_interval(?TASK_FETCH_INTERVAL, poll),
    request_task(),
    {noreply, {#state{work_state = no_task, timer = Timer}, {NewUp, NewDown}}};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles errournous exit of user application.
%% Will tell the user through a user_info message.
%%
%% @spec handle_cast({Pid, error, CallState}, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({_Pid, error,  {JobId, _TaskId, Time, TaskType, _Progname}},
	    {_Timer, {OldUp, OldDown}}) ->
    %% Report to statistician
    Diff = timer:now_diff(now(), Time) / 1000000,
    Power = power_check:get_watt_per_task(Diff),
    {NewUp, NewDown} = netMonitor:get_net_stats(),
    Disk = statistician:get_node_disk_usage(raw),
    Mem = statistician:get_node_mem_usage(raw),
    statistician:update({{node(), JobId, list_to_atom(TaskType)},
                         Power, Diff, NewUp - OldUp, NewDown - OldDown, 0, 1,
                         Disk, Mem}),
    
    %% Free task that has been given to node
    dispatcher:task_failed(JobId, list_to_atom(TaskType)),

    %% Kill and remove computingProcess spec from dynamic supervisor
    supervisor:terminate_child(?DYNSUP, ?WORKER),
    supervisor:delete_child(?DYNSUP, ?WORKER),

    %% Reinstate poll timer and request task
    {ok, Timer} = timer:send_interval(?TASK_FETCH_INTERVAL, poll),
    request_task(),
    {noreply, {#state{work_state = no_task, timer = Timer}, {NewUp, NewDown}}};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w : Received unexpected handle_cast call.~n"
                       "Message: ~p~n",
                       [?MODULE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(poll, {State = #state{work_state = no_task}, NetLoad}) ->
    request_task(),
    {noreply, {State, NetLoad}};
handle_info({task_response, no_task}, {State, NetLoad}) ->
    {noreply, {State, NetLoad}};
handle_info({task_response, Task}, {State, NetLoad}) ->
    start_task(Task),
    timer:cancel(State#state.timer),
    {noreply, {State#state{work_state = task}, NetLoad}};
handle_info(stop_job, {State, NetLoad}) ->
    chronicler:debug("~w : Stopping job~n", [?MODULE]),
    computingProcess:stop(),
    {NewUp, NewDown} = netMonitor:get_net_stats(),
    {ok, Timer} = timer:send_interval(?TASK_FETCH_INTERVAL, poll),
    {noreply, {#state{work_state = no_task, timer = Timer}, {NewUp, NewDown}}};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    chronicler:warning("~w : Received unexpected handle_info call.~n"
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
    chronicler:info("~w : Received terminate call.~n"
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
    chronicler:debug("~w : Received unexpected code_change call.~n"
                     "Old version: ~p~n"
                     "Extra: ~p~n",
                     [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Polls the dispatcher for a free task.
%%
%% @spec request_task() -> {Task, NewState}
%% @end
%%--------------------------------------------------------------------
request_task() ->
    dispatcher:fetch_task(node(), self()).

%%--------------------------------------------------------------------
%% @doc
%% Tells the dispatcher to create a new task
%%
%% @spec give_task(Id, Type, Path) -> {Task, NewState}
%% @end
%%--------------------------------------------------------------------
give_task({JobId, _TaskId, _Time, _OldType, Progname}, Type, Path) ->
    dispatcher:add_task({JobId, Progname, Type,
			    "tmp/" ++ integer_to_list(JobId) ++ Path}).

%%--------------------------------------------------------------------
%% @doc
%% Starts an already fetched task on the node.
%%
%% @spec start_task({TaskId, JobId, Op, Path, Name}) -> ChildSpec
%% @end
%%--------------------------------------------------------------------

start_task({_Lool, TaskId, JobId, Name, Op, Path, _OLOLOL}) ->
    ChildSpec = {?WORKER,
		 {?WORKER,
		  start_link,
		  [Name, Op, JobId, Path, TaskId]},
		 transient,
		 1,
		 worker,
		 [?WORKER]},
    supervisor:start_child(?DYNSUP, ChildSpec),
    ChildSpec.
