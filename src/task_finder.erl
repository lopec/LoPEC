%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson <>
%%% @doc
%%% Finds a task and returns a task in the same way as db:get_task()
%%% does.
%%% @end
%%% Created : 30 Sep 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(task_finder).
-behaviour(gen_server).

%% API
-export([start_link/0, find_task/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% Internal
-export([get_first_available/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, task_finder}, task_finder, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Finds an available task
%%
%% @spec find_task(JobId) -> {see_gen_server_cast}
%% @end
%%--------------------------------------------------------------------
find_task(JobId) ->
    gen_server:call(task_finder, {find_task, JobId}).


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
init([]) ->
    {ok, []}.


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
handle_call({find_task, JobId}, _From, State) ->
    {reply,get_first_available(db:list_tasks(JobId),available),State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% Receives data from task listener and adds it to task list
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> 
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
handle_info(_Info, State) -> %not used
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> %not used
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> %not used
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_first_available([], _) ->
    [];
get_first_available([H|T], State) ->
    {task_state, _} = db:get_task_state(H),
    if task_state == State ->
	    db:get_task(H);
       true ->
	    get_first_available(T, State)
    end.
