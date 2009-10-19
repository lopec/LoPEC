%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman & Fredrik Andersson <sedrik@consbox.se>
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
-define(TIMEOUT, 1000).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%--------------------------------------------------------------------
%% @doc
%% Saves a new job to the database
%%
%% @spec new_task(Id, Type, Path) -> Task | {error, Error}
%% @end
%%--------------------------------------------------------------------
new_task(Id, Type, Path) ->
    gen_server:call(?MODULE, {request, new_task, Id, Type, Path}).

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
    {ok, TimerRef} = timer:send_interval(1000, poll),
    {ok, #state{timer = TimerRef}}.

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
handle_call({request, new_task, Id, Type, Path}, _From, State) ->
    Reply = give_task(Id, Type, Path),
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
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_call call.~n"
                         "Message: ~p~n"
                         "From: ~p~n",
                         [?MODULE, Msg, From])),
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
handle_cast({_Pid, _ExitStatus, {_JobId, TaskId}}, State) ->
    supervisor:terminate_child(?DYNSUP, ?WORKER),
    supervisor:delete_child(?DYNSUP, ?WORKER),
    dispatcher:report_task_done(TaskId),
    chronicler:info(io_lib:format("ololo: ~p~n", [State])),
    {ok, Timer} = timer:send_interval(1000, poll),
    request_task(),
    {noreply, #state{work_state = no_task, timer = Timer}};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_cast call.~n"
                         "Message: ~p~n",
                         [?MODULE, Msg])),
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
handle_info(poll, State = #state{work_state = no_task}) ->
    request_task(),
    {noreply, State};
handle_info({task_response, Task}, State) ->
    start_task(Task),
    timer:cancel(State#state.timer),
    {noreply, State#state{work_state = task}};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_info call.~n"
                         "Info: ~p~n",
                         [?MODULE, Info])),
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
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected terminate call.~n"
                         "Reason: ~p~n",
                         [?MODULE, Reason])),
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
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected code_change call.~n"
                         "Old version: ~p~n"
                         "Extra: ~p~n",
                         [?MODULE, OldVsn, Extra])),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a free task, if no free tasks exists the master will be contacted to
%% request more tasks.
%%
%% @spec request_task() -> {Task, NewState}
%% @end
%%--------------------------------------------------------------------
request_task() ->
    dispatcher:get_task(node(), self()).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new task in the database.
%%
%% @spec give_task(Id, Type, Path) -> {Task, NewState}
%% @end
%%--------------------------------------------------------------------

give_task({JobId, _TaskId}, Type, Path) ->
    dispatcher:create_task({JobId, Type,
			    "tmp/" ++ integer_to_list(JobId) ++ Path, 1}).

%%--------------------------------------------------------------------
%% @doc
%% Starts a already fetched task on the node.
%%
%% @spec start_task({TaskId, JobId, Op, Path, Name}) -> ChildSpec
%% @end
%%--------------------------------------------------------------------

start_task({_Lool, TaskId, JobId, Op, Path, Name}) ->
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
