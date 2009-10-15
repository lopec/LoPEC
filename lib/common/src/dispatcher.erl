%%%-------------------------------------------------------------------
%%% @author Axel Andrén, Vasilij Savin
%%% <axelandren@gmail.com, vasilij@savin.com>
%%% @copyright (C) 2009, Axel Andrén
%%% @doc
%%% 
%%% Interfaces with the database. Can take requests for tasks, marking
%%% a task as done, adding tasks or jobs, and free tasks assigned to
%%% nodes (by un-assigning them).
%%% 
%%% @end
%%% Created : 30 Sep 2009 by Axel Andrén <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(dispatcher).
-behaviour(gen_server).
-include("../../master/include/db.hrl").

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
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Adds specified task to the database task list.
%% <pre>
%% TaskSpec is a tuple:
%% {
%%     JobId,
%%     Tasktype,  % atoms 'map', 'reduce', 'finalise' or 'split' are
%%                  accepted at the moment (without quote marks '')
%%     input_path,% input file name
%%     priority   % not yet implemented
%% }
%% </pre>
%% @spec create_task(TaskSpec) -> TaskID
%% @end
%%--------------------------------------------------------------------
create_task(TaskSpec) ->
    gen_server:call({global, ?MODULE}, {create_task, TaskSpec}).

%%--------------------------------------------------------------------
%% @doc
%% Frees all tasks assigned to Node in master task list
%%
%% @spec free_tasks(NodeID) -> ok
%% @end
%%--------------------------------------------------------------------
free_tasks(NodeId) ->
    gen_server:cast({global, ?MODULE}, {free_tasks, NodeId}).

%%--------------------------------------------------------------------
%% @doc
%% Adds specified job to the database job list.
%% <pre>
%% JobSpec is a tuple:
%% {   
%%     JobType, % atoms 'map', 'reduce', 'finalise' or 'split' are
%%                accepted at the moment (without quote marks '')
%%     priority % not implemented at the moment
%% }
%% </pre>
%% @spec add_job(JobSpec) -> JobID
%% @end
%%--------------------------------------------------------------------
add_job(JobSpec) ->
    gen_server:call({global, ?MODULE}, {create_job, JobSpec}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to the caller with the first available task.
%% If no tasks are available, we just let the request time out.
%%
%% @spec get_task(NodeID, PID) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
get_task(NodeId, PID) ->
    gen_server:cast({global, ?MODULE}, {task_request, NodeId, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Marks the task as being completely done. The results should be
%% posted on storage before calling this method. 
%% 
%% @spec report_task_done(TaskID) -> {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
report_task_done(TaskId) ->
    gen_server:call({global, ?MODULE}, {task_done, TaskId, no_task}).

%%--------------------------------------------------------------------
%% @doc
%% Like report_task_done/1 except the node can ask to generate another
%% task by providing a TaskSpec
%%
%% @spec report_task_done(TaskID, TaskSpec) -> {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
report_task_done(TaskId, TaskSpec) ->
    gen_server:call({global, ?MODULE}, {task_done, TaskId, TaskSpec}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server. Currently no special setup is needed.
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([]) ->
    application:start(chronicler),
    chronicler:info(io_lib:format("dispatcher: Application started~n", [])),
    {ok, []}.



%%--------------------------------------------------------------------
%% @doc
%% Expects task requests from nodes, and passes such requests to the
%% find_task function.
%%
%% @spec handle_cast({task_request, NodeId, From}, State) ->
%%                                                    {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({task_request, NodeId, From}, _State) ->
    spawn(?MODULE, find_task, [From, NodeId]),
    {noreply, []};
%%--------------------------------------------------------------------
%% @doc
%% Un-assigns all tasks assigned to the specified node.
%%
%% @spec handle_cast({free_tasks, NodeId}, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({free_tasks, NodeId}, State) ->
    db:free_tasks(NodeId),
    {noreply, State};
%%--------------------------------------------------------------------
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:debug(io_lib:format(
		       "dispatcher: Wrong message received: ~p~n", [Msg])),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @doc
%% Marks a specified task as done in the database.
%%
%% @spec handle_call({task_done, TaskId, no_task}, From, State) ->
%%                                   {reply, ok, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({task_done, TaskId, no_task}, _From, State) ->
    db:mark_done(TaskId),
    {reply, ok, State};
%%--------------------------------------------------------------------
%% @doc
%% Marks a specified task as done in the database and adds a
%% (different) specified task to the database
%%
%% @spec handle_call({task_done, TaskId, TaskSpec}, From, State) ->
%%                                   {reply, NewTaskId, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({task_done, TaskId, TaskSpec}, _From, State) ->
    db:mark_done(TaskId),
    NewTaskId = db:add_task(TaskSpec),
    {reply, NewTaskId, State};
%%--------------------------------------------------------------------
%% @doc
%% Adds a specified task to the database
%%
%% @spec handle_call({create_task, TaskSpec}, From, State) ->
%%                                   {reply, NewTaskID, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({create_task, TaskSpec}, _From, State) ->
    NewTaskId = db:add_task(TaskSpec),
    {reply, NewTaskId, State};
%%--------------------------------------------------------------------
%% @doc
%% Adds a specified job to the database
%%
%% @spec handle_call({create_job, JobSpec}, From, State) ->
%%                                   {reply, NewJobID, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({create_job, JobSpec}, _From, State) ->
    NewJobId = db:add_job(JobSpec),
    {reply, NewJobId, State}.




%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends a message to given PID with the first found task in the DB,
%% and tells the ECG to register this new node with that PID. If no
%% task is found, it terminates and lets request time out.
%%
%% @spec find_task(RequesterPID, NodeId) -> ok | db:assign_task()
%% @end
%%--------------------------------------------------------------------
find_task(RequesterPID, NodeId) ->
    FreeTask = db:get_task(NodeId),
    %chronicler:info(io_lib:format("dispatcher: Received task: ~p~n", 
    %                   [FreeTask])),
    case FreeTask of
        % If no task found - let the request time out and try again
        % Therefore we just terminate
        no_task -> 
            ok;
        Task ->
            Job = db:get_job_info(Task#task.job_id),
            chronicler:info(io_lib:format("dispatcher: Received job: ~p~n", 
                       [Job])),
            JobType = Job#job.job_type,
            AssignedTask = #task_tmp {task_id = Task#task.task_id,
                            job_id = Task#task.job_id,
                            task_type = Task#task.task_type,
                            input_file = Task#task.input_path,
                            job_type = JobType},
            RequesterPID ! {task_response, AssignedTask},
            ecg_server:accept_message({new_node, NodeId}),
            db:assign_task(Task#task.task_id, NodeId)
    end.

%%%===================================================================
%%% Not implemented stuff
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% (Template default)
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
%% (Template default)
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
%% (Template default)
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) -> %template default
    {noreply, State}.
