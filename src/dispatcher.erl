%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Receives job/task requests from a node, and returns the first
%%% available job to it. If no jobs are found, the first available
%%% task is instead returned. If there are no jobs or tasks, it
%%% returns what db:get_task() returns in that case - the atom
%%% no_task, presumably.
%%% @end
%%% Created : 30 Sep 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher).

-behaviour(gen_server).

%% API
-export([start_link/0, get_work/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, find_job/1, find_task/1]).

-define(SERVER, dispatcher_server). 


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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Returns the first available job, or if no jobs are available, the
%% first available task, or if no tasks are available,
%% no_task. Assuming db:get_task() returns no_task, and db:get_job()
%% no_job (when applicable for both).
%%
%% @spec get_work() -> {ok, JobID} | {ok, TaskID} | no_task
%% @end
%%--------------------------------------------------------------------
get_work() ->
    gen_server:cast({get_job, self()}, waiting_for_requests).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, waiting_for_requests}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, ok, State} 
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Expects job requests from nodes, and passes such requests to the
%% two internal functions find_job/1 and possibly find_task/1.
%%
%% @spec handle_cast(Msg, State) -> ok | {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({job_req, From}, waiting_for_requests) ->
    spawn(?MODULE, find_job, [From]);
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) -> %template default
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function.
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> %template default
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function.
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> %template default
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sends a message to given PID with the first found job in the DB,
%% and tells the ECG to register this new node with that PID.  If no
%% job is found, it instead calls find_task/1 with the given PID.
%%
%% @spec find_job(From) -> ok
%% @end
%%--------------------------------------------------------------------
find_job(From) ->
    case db:get_job() of
        no_job -> find_task(From);
        Job -> 
            global:send(ecg_server, {new_node, From}),
            From ! {new_job, Job}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sends a message to given PID with the first found task in the DB,
%% and tells the ECG to register this new node with that PID. If no
%% task is found, it instead sends {no_task_found}.
%%
%% @spec find_task(From) -> ok
%% @end
%%--------------------------------------------------------------------
find_task(From) ->
    case db:get_task() of
        no_task -> From ! {no_task_found};
        Task -> 
            global:send(ecg_server, {new_node, From}),
            From ! {new_task, Task}
    end.
