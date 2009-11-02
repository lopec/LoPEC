%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @author Vasilij Savin <vasilij.savin@gmail.com>
%%% @copyright (C) 2009, Axel Andren
%%% @doc
%%% 
%%% Interfaces with the database. Can take requests for tasks, marking
%%% a task as done, adding tasks or jobs, and free tasks assigned to
%%% nodes (by un-assigning them).
%%% 
%%% @end
%%% Created : 30 Sep 2009 by Axel Andren
%%%-------------------------------------------------------------------
-module(dispatcher).
-behaviour(gen_server).
-include("../../master/include/db.hrl").
-include("../../master/include/global.hrl").

%% API
-export([start_link/0,
         add_job/1,
         add_task/1, 
         fetch_task/2,
         report_task_done/2, 
         report_task_done/1,
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
%%      JobId,
%%      ProgramName,
%%      Type - atoms 'map', 'reduce', 'finalize' or 'split' are
%%                  accepted at the moment (without quote marks '')
%%      Path - input file name
%% }
%% </pre>
%% @spec add_task(TaskSpec) -> TaskID
%% @end
%%--------------------------------------------------------------------
add_task(TaskSpec) ->
    gen_server:call({global, ?MODULE}, {add_task, TaskSpec}).

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
%%      ProgramName,
%%      ProblemType (map reduce only accepted now)
%%      Owner
%%      Priority - not implemented at the moment
%% }
%% </pre>
%% @spec add_job(JobSpec) -> JobID
%% @end
%%--------------------------------------------------------------------
add_job(JobSpec) ->
    gen_server:call({global, ?MODULE}, {add_job, JobSpec}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to the caller with the first available task.
%% If no tasks are available, we just let the request time out.
%%
%% @spec fetch_task(NodeID, PID) -> ok
%% @end
%%--------------------------------------------------------------------
fetch_task(NodeId, PID) ->
    gen_server:cast({global, ?MODULE}, {task_request, NodeId, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Marks the task as being completely done. The results should be
%% posted on storage before calling this method. 
%% 
%% @spec report_task_done(TaskID) -> ok
%% @end
%%--------------------------------------------------------------------
report_task_done(TaskId) ->
    gen_server:call({global, ?MODULE}, {task_done, TaskId, no_task}).

%%--------------------------------------------------------------------
%% @doc
%% Like report_task_done/1 except the node can ask to generate another
%% task by providing a TaskSpec
%%
%% @spec report_task_done(TaskID, TaskSpec) -> ok
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
    chronicler:info("~p: Application started~n", [?MODULE]),
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
handle_cast({task_request, NodeId, From}, State) ->
    spawn(?MODULE, find_task, [From, NodeId]),
    {noreply, State};
%%--------------------------------------------------------------------
%% @doc
%% Un-assigns all tasks assigned to the specified node.
%%
%% @spec handle_cast({free_tasks, NodeId}, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({free_tasks, NodeId}, State) ->
    Jobs = db:free_tasks(NodeId),
    chronicler:debug("Node ~p" "freed the tasks: ~p", [NodeId, Jobs]),
    examiner:report_free(Jobs),
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
%% @doc
%% Marks a specified task as done in the database.
%%
%% @spec handle_call({task_done, TaskId, no_task}, From, State) ->
%%                                   {reply, ok, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({task_done, TaskId, no_task}, _From, State) ->
    mark_done(TaskId),
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
    chronicler:debug("TaskSpec: ~p", [TaskSpec]),
    NewTaskId = create_task(TaskSpec),
    mark_done(TaskId),
    {reply, NewTaskId, State};
%%--------------------------------------------------------------------
%% @doc
%% Adds a specified task to the database
%%
%% @spec handle_call({create_task, TaskSpec}, From, State) ->
%%                                   {reply, NewTaskID, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({add_task, TaskSpec}, _From, State) ->
    NewTaskId = create_task(TaskSpec),
    chronicler:debug("Created: ~p", [NewTaskId]),
    {reply, NewTaskId, State};

%%--------------------------------------------------------------------
%% @doc
%% Adds a specified job to the database
%%
%% @spec handle_call({create_job, JobSpec}, From, State) ->
%%                                   {reply, NewJobID, State} 
%% @end
%%--------------------------------------------------------------------
handle_call({add_job, JobSpec}, _From, State) ->
    chronicler:debug("JobSpec: ~p", [JobSpec]),
    NewJobId = db:add_job(JobSpec),
    chronicler:debug("JobId: ~p", [NewJobId]),
    examiner:insert(NewJobId),
    chronicler:debug("Job Examiner: ~p", [examiner:get_progress(NewJobId)]),
    {reply, NewJobId, State};
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
    FreeTask = db:fetch_task(NodeId),
    case FreeTask of
        % If no task found - let the request time out and try again
        % Therefore we just terminate
        no_task ->
            chronicler:debug("~p: Found no tasks.~n",[?MODULE]),
            ok;
        Task ->
            chronicler:debug("~p: Found task ~p.~n",
                             [?MODULE, Task#task.task_id]),
            RequesterPID ! {task_response, Task},
            ecg_server:accept_message({new_node, NodeId}),
            chronicler:debug("Assign Examiner: ~p",
                             [examiner:get_progress(Task#task.job_id)]),
            examiner:report_assigned(Task#task.job_id, Task#task.type)
    end.

mark_done(TaskId) ->
    db:mark_done(TaskId),
    Task = db:get_task(TaskId),
    chronicler:debug("Done Examiner: ~p",
                     [examiner:get_progress(Task#task.job_id)]),
    examiner:report_done(Task#task.job_id, Task#task.type).

create_task(TaskSpec) ->
    chronicler:debug("TaskSpec: ~p", [TaskSpec]),
    case db:add_task(TaskSpec) of
        task_not_added ->
            chronicler:debug
                ("Duplicate task was not created", []);
        NewTaskId ->
            Task = db:get_task(NewTaskId),
            chronicler:debug
                ("Create: ~p", [examiner:get_progress(Task#task.job_id)]),
            examiner:report_created(Task#task.job_id, Task#task.type),
            NewTaskId
    end.

%%%===================================================================
%%% Not implemented stuff
%%%===================================================================

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
    chronicler:debug("~w:Received code_change call.~n"
                     "Old version: ~p~n"
                     "Extra: ~p~n",
                     [?MODULE, OldVsn, Extra]),
    {ok, State}.

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
