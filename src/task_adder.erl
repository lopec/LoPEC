%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Receives tasks from the task listener and adds them to the task
%%% list. Multiple copies of this process may exist simultaneously.
%%%
%%% @end
%%% Created : 30 Sep 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(task_adder).
-behaviour(gen_server).


%% API
-export([start_link/0, accept_job/1, create_task/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% JobSpec:
%%  {   new_job,
%%      <job_type> - which computational program to run
%%      <data_path>, - path to datafile in storage 
%%      <script_cmd> - command to run split script
%%      <split_script_path>, - path to split_script in storage
%%      <priortity> - not implemented at the moment 
%%    }
%%
%% The first task of the job would be to run split script on node.
%% Format of that command is "<script_cmd> <split_script_path>".
%% @end
%%--------------------------------------------------------------------
accept_job(JobSpec) ->
    gen_server:call(?MODULE, {create_job, JobSpec}).

%%--------------------------------------------------------------------
%% @doc
%% TaskSpec:
%%  {   add_task,
%%      <JobId>
%%      <Tasktype> - map, reduce or split atoms accepted at the moment 
%%      <data_path>, - path to datafile in storage 
%%      <callback_path>, - path to implementing library/function in storage
%%      <priortity> - not implemented at the moment
%%    }
%%
%% The first task of the job would be to run split script on node.
%% Format of that command is "<script_cmd> <split_script_path>".
%% @end
%%--------------------------------------------------------------------
create_task(TaskSpec) ->
    gen_server:call(?MODULE, {create_task, TaskSpec}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server.
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, waiting_for_task_details}.

%%--------------------------------------------------------------------
%% @doc
%%
%% Adds given data to the DB. The result of this operation is sent as
%% a message to the ID what sent the data to be added to the DB.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({create_task, TaskSpec}, _From, State) ->
    NewTaskId = db:add_task(TaskSpec),
    {reply, NewTaskId, State}; 
handle_call({create_job, JobSpec}, _From, State) ->
    NewJobId = db:add_job(JobSpec),
    {reply, NewJobId, State}; 
handle_call(Request, From, State) ->
    logger:error("task_adder:handle_call got an invalid call!~n"
                 "--Request: ~p~n"
                 "--From: ~p~n"
                 "--State: ~p~n", [Request, From, State]),
    Reply = ok,
    {noreply, Reply, State}.



%%%===================================================================
%%% Not implemented stuff
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) -> %template default
    logger:error("task_adder:handle_cast got an unexpected call!~n"
                 "--Msg: ~p~n"
                 "--State: ~p~n", [Msg, State]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> %template default
    logger:error("task_adder:handle_info got an unexpected call!~n"
                 "--Info: ~p~n"
                 "--State: ~p~n", [Info, State]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) -> %template default
    logger:error("task_adder:terminate got an unexpected call!~n"
                 "--Reason: ~p~n"
                 "--State: ~p~n", [Reason, State]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Not implemented, not expecting any calls to this function
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> %template default
    logger:error("task_adder:code_change got an unexpected call!~n"
                 "--Old version: ~p~n"
                 "--State: ~p~n"
                 "--Extra: ~p~n", [OldVsn, State, Extra]),
    {ok, State}.