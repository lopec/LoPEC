%%%-------------------------------------------------------------------
%%% @author Axel, Vasilij Savin <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Receives task requests from a node, and returns the first
%%% available task to it. If there is no available task, it
%%% does not return and let request time out.
%%% Also listens to reports from nodes and marks tasks as completed.
%%% @end
%%% Created : 30 Sep 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher).
-behaviour(gen_server).
-include("../include/db.hrl").

%% API
-export([start_link/0, 
         get_task/2,
         report_task_done/2, 
         create_task/1, 
         report_task_done/1,
         add_job/1,
         free_tasks/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, find_task/2]). 

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
%% TaskSpec:
%%  {   'JobId',
%%      'Tasktype' - map, reduce, finalise or split atoms accepted at the moment 
%%      'priority' - not implemented at the moment
%%    }
%% Returns TaskId of newly created task
%% @end
%%--------------------------------------------------------------------
create_task(TaskSpec) ->
    gen_server:call(?MODULE, {create_task, TaskSpec}).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Frees all tasks assigned to Node in master task list
%% @end
%%--------------------------------------------------------------------
free_tasks(NodeId) ->
    gen_server:call(?MODULE, {free_tasks, NodeId}).

%%--------------------------------------------------------------------
%% @doc
%% TaskSpec:
%%  {   
%%      'JobType' - map, reduce, finalise or split atoms accepted at the moment 
%%      'priority' - not implemented at the moment
%%    }
%% Returns JobId of newly created job
%% @end
%%--------------------------------------------------------------------
add_job(JobSpec) ->
    gen_server:call(?MODULE, {create_job, JobSpec}).

%%--------------------------------------------------------------------
%% @doc
%% Returns the first available task.
%% If no tasks are available, just lets request to time out.
%%
%% @end
%%--------------------------------------------------------------------
get_task(NodeId, PID) ->
    gen_server:cast(?MODULE, {task_request, NodeId, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Marks the task as being completely done. The results should be
%% posted on storage before calling this method.
%% Also, node can ask to generate another task by providing TaskSpec
%% @end
%%--------------------------------------------------------------------
report_task_done(TaskId) ->
    gen_server:call(?MODULE, {task_done, TaskId, no_task}).
report_task_done(TaskId, TaskSpec) ->
    gen_server:call(?MODULE, {task_done, TaskId, TaskSpec}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server. Currently no special setup is needed.
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([]) ->
    register (logger, spawn_link(logger_ext, start, ["test.logging"])),
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc
%%
%% Expects task requests from nodes, and passes such requests to the
%% find_task function.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast({task_request, NodeId, From}, _State) ->
    spawn(?MODULE, find_task, [From, NodeId]),
    {noreply, []};
handle_cast({free_tasks, NodeId}, _State) ->
    db:free_tasks(NodeId);
handle_cast(Msg, State) ->
    io:format("Wrong message received: ~w", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, ok, State} |
%%                                   {reply, NewTaskId, State} |
%%                                   {reply, NewJobId, State}
%% @end
%%--------------------------------------------------------------------
handle_call({task_done, TaskId, no_task}, _From, State) ->
    db:mark_done(TaskId),
    {reply, ok, State};
handle_call({task_done, TaskId, TaskSpec}, _From, State) ->
    db:mark_done(TaskId),
    NewTaskId = db:add_task(TaskSpec),
    {reply, NewTaskId, State};
handle_call({create_task, TaskSpec}, _From, State) ->
    NewTaskId = db:add_task(TaskSpec),
    {reply, NewTaskId, State};
handle_call({create_job, JobSpec}, _From, State) ->
    NewJobId = db:add_job(JobSpec),
    {reply, NewJobId, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @TODO fix this comment
%% Sends a message to given PID with the first found task in the DB,
%% and tells the ECG to register this new node with that PID. If no
%% task is found, it terminates and lets request time out.
%%
%% @spec find_task(RequesterPID, NodeId) -> ok
%% @end
%%--------------------------------------------------------------------
find_task(RequesterPID, NodeId) ->
    FreeTask = db:get_task(NodeId),
    case FreeTask of
        % If no task found - let the request time out and try again
        % Therefore we just terminate
        no_task -> 
            ok;
        Task ->
            % Get ready with integration testing!
            ecg_server:accept_message({new_node, NodeId}),
            RequesterPID ! {task_response, Task, self()},
            db:assign_task(Task#task.task_id, NodeId)
    end.

%%%===================================================================
%%% Not implemented stuff
%%%===================================================================
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
