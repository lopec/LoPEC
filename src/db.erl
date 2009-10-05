%%%-------------------------------------------------------------------
%%% File    : db.erl
%%% Author  : Henkan <henkan@SKYNET>
%%% Description : 
%%%
%%% Created : 30 Sep 2009 by Henkan <henkan@SKYNET>
%%%-------------------------------------------------------------------

%%% Some extra info: the first time you start the database,
%%% type db:start() in the erlang shell, followed by
%%% db:create_tables(). This will setup the necessary things
%%% for running the shiznit. When starting it otherwise,
%%% all you need to do is start the mnesia application
%%% with db:start(). Gl hf!

-module(db).
-include("../include/db.hrl").
-behaviour(gen_server).
-define(SERVER, db_server).

%% APIs for management of the databases
-export([start/0, start_link/0, stop/0, create_tables/0]).

%% APIs for external access to the job table
-export([add_job/4, remove_job/1, 
	 get_job/0, get_job_info/1, get_job_reply_id/1,
	 get_job_callback_path/1, get_job_input_path/1,
	 get_job_state/1, get_job_priority/1, get_job_progress/1,
	 set_job_state/2, set_job_priority/2, set_job_progress/2,
	 list_jobs/0]).

%% APIs for external access to the task table
-export([add_task/5, remove_task/1,
	 get_task/0, get_task_info/1,
	 get_task_callback_path/1, get_task_input_path/1,
	 get_task_state/1, get_task_priority/1, get_task_type/1, 
	 set_task_state/2, set_task_priority/2,
	 list_tasks/0, list_tasks/1, list_node_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Starts the database gen_server.
%%
%% @spec start()
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Starts the database gen_server.
%%
%% @spec start_link()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Stops the database gen_server.
%%
%% @spec stop()
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Creates the tables used for keeping track of the jobs and
%% tasks.
%%
%% @spec create_tables()
%% @end
%%--------------------------------------------------------------------
create_tables() ->
    gen_server:call(?SERVER, create_tables).


%%====================================================================
%% JOB TABLE APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a job to the job table.
%% 
%% @spec add_job() -> ok
%% @end
%%--------------------------------------------------------------------
add_job(CallbackPath, InputPath, ReplyId, Priority) ->
    gen_server:call(?SERVER, {add_job, CallbackPath, InputPath,
					ReplyId, Priority}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Removes a job from the job table.
%% 
%% @spec remove_job() -> ok
%% @end
%%--------------------------------------------------------------------
remove_job(JobId) ->
    gen_server:call(?SERVER, {remove_job, JobId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a job from the job table which has its current status set
%% to 'available'.
%% 
%% @spec get_job() -> Record
%% @end
%%--------------------------------------------------------------------
get_job() ->
    gen_server:call(?SERVER, {get_job}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the full record of a job given its 'JobId'.
%% 
%% @spec get_job_info(JobId) -> Record
%% @end
%%--------------------------------------------------------------------
get_job_info(JobId) ->
    gen_server:call(?SERVER, {get_job_info, JobId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Changes the state of the given job with id 'JobId' to 'State'.
%% 
%% @spec set_job_state() -> ok
%% @end
%%--------------------------------------------------------------------
set_job_state(JobId, State) ->
    gen_server:call(?SERVER, {set_job_state, JobId, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Changes the progress of the given job with id 'JobId' to 'Progress'.
%% 
%% @spec set_job_progress() -> ok
%% @end
%%--------------------------------------------------------------------
set_job_progress(JobId, Progress) ->
    gen_server:call(?SERVER, {set_job_progress, JobId, Progress}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Changes the priority of the given job with id 'JobId' to 'Priority'.
%% 
%% @spec set_job_priority() -> ok
%% @end
%%--------------------------------------------------------------------
set_job_priority(JobId, Priority) ->
    gen_server:call(?SERVER, {set_job_priority, JobId, Priority}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the state of the given job with id 'JobId'.
%% 
%% @spec get_job_state() -> State
%% @end
%%--------------------------------------------------------------------
get_job_state(JobId) ->
    Job = get_job_info(JobId),
    Job#job.current_state.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the progress of the given job with id 'JobId'.
%% 
%% @spec get_job_progress() -> Progress
%% @end
%%--------------------------------------------------------------------
get_job_progress(JobId) ->
    Job = get_job_info(JobId),
    Job#job.current_progress.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the callback path of the given job with id 'JobId'.
%% 
%% @spec get_callback_path() -> CallbackPath
%% @end
%%--------------------------------------------------------------------
get_job_callback_path(JobId) ->
    Job = get_job_info(JobId),
    Job#job.callback_path.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the input path of the job with id 'JobId'.
%% 
%% @spec get_job_input_path() -> InputPath
%% @end
%%--------------------------------------------------------------------
get_job_input_path(JobId) ->
    Job = get_job_info(JobId),
    Job#job.input_path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the reply id of the job with id 'JobId'.
%% 
%% @spec get_job_reply_id() -> ReplyId
%% @end
%%--------------------------------------------------------------------
get_job_reply_id(JobId) ->
    Job = get_job_info(JobId),
    Job#job.reply_id.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the priority of the job with id 'JobId'.
%% 
%% @spec get_job_priority() -> Priority
%% @end
%%--------------------------------------------------------------------
get_job_priority(JobId) ->
    Job = get_job_info(JobId),
    Job#job.priority.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a list containing all id's of the jobs in the job table.
%% 
%% @spec list_jobs() -> ListOfJobIds
%% @end
%%--------------------------------------------------------------------
list_jobs() ->
    gen_server:call(?SERVER, {list_jobs}).

%%====================================================================
%% TASK TABLE APIs
%%====================================================================

add_task(JobId, TaskType, CallbackPath, InputPath, Priority) ->
    gen_server:call(?SERVER, {add_task, JobId, 
					TaskType, CallbackPath, 
					InputPath, available, Priority}).

remove_task(TaskId) ->
    gen_server:call(?SERVER, {remove_task, TaskId}).

get_task() ->
    gen_server:call(?SERVER, {get_task}).

get_task_info(TaskId) ->
    gen_server:call(?SERVER, {get_task_info, TaskId}).

set_task_state(TaskId, State) ->
    gen_server:call(?SERVER, {set_task_state, TaskId, State}).

set_task_priority(TaskId, Priority) ->
    gen_server:call(?SERVER, {set_task_priority, TaskId, Priority}).
 
get_task_state(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.current_state.

get_task_callback_path(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.callback_path.    

get_task_input_path(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.input_path.

get_task_type(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.task_type.

get_task_priority(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.priority.

list_tasks() ->
    gen_server:call(?SERVER, {list_tasks}).

list_tasks(JobId) ->
    gen_server:call(?SERVER, {list_tasks, JobId}).

list_node_tasks(NodeId) ->
    gen_server:call(?SERVER, {list_node_tasks, NodeId}).
		
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
    NodeList = [node()],             %TODO: add distribution to other
    mnesia:create_schema([node()]),  %database nodes!
    application:start(mnesia),
    {ok, NodeList}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(init_db, _From, State) ->
    mnesia:create_schema([node()|nodes()]),
    {reply, database_created, State};

handle_call(create_tables, _From, State) ->
    Opts = [{type, set}, {disc_copies, [node()]}],
    {atomic, ok} = mnesia:create_table(
		     job, [{attributes, record_info(fields, job)}|Opts]),
    {atomic, ok} = mnesia:create_table(
		     task, [{attributes, record_info(fields, task)}|Opts]),
    {atomic, ok} = mnesia:create_table(
		     task_job, [{attributes, record_info(fields, task_job)}|Opts]),
    {reply, tables_created, State};

handle_call({add_job,CallbackPath, InputPath, ReplyId, Priority}, 
	    _From, State) ->
    JobId = generate_id(),
    add_job_on_server(JobId, CallbackPath, InputPath, available, undefined,
		   ReplyId, Priority),
    {reply, JobId, State};

handle_call({remove_job, JobId}, _From, State) ->
    remove_job_on_server(JobId),
    {reply, ok, State};

handle_call({get_job}, _From, State) ->
    F = fun() ->
		MatchHead = #job{job_id = '$1',
				 callback_path = '_',
				 input_path = '_',
				 current_state = available,
				 current_progress = '_',
				 reply_id = '_',
				 priority = '_'},
		Result = '$1',
	        mnesia:select(job, [{MatchHead, [], [Result]}], 1, read)
	end,

    Result = mnesia:transaction(F),
    case Result of
	{atomic, {[Job], _Cont}} ->
 	    {reply, get_job_info_on_server(Job), State};
 	{atomic, '$end_of_table'} ->
 	    {reply, no_job, State}
     end;

handle_call({get_job_info, JobId}, _From, State) ->
    Job = get_job_info_on_server(JobId),
    {reply, Job, State};

handle_call({set_job_state, JobId, State}, _From, State) ->
     F = fun() ->
 		Job = get_job_info_on_server(JobId),
 		remove_job_on_server(JobId),
 		add_job_on_server(JobId, 
 			Job#job.callback_path, 
 			Job#job.input_path,
 		        State, 
 			Job#job.current_progress, 
 			Job#job.reply_id,
 			Job#job.priority)
 	end,
     mnesia:transaction(F),
    {reply, ok, State};

handle_call({set_job_progress, JobId, Progress}, _From, State) ->
     F = fun() ->
 		Job = get_job_info_on_server(JobId),
 		remove_job_on_server(JobId),
 		add_job_on_server(JobId, 
 			Job#job.callback_path, 
 			Job#job.input_path,
 			Job#job.current_state, 
 			Progress, 
 			Job#job.reply_id,
 		        Job#job.priority)
 	end,
     mnesia:transaction(F),
    {reply, ok, State};

handle_call({set_job_priority, JobId, Priority}, _From, State) ->
     F = fun() ->
 		Job = get_job_info_on_server(JobId),
 		remove_job_on_server(JobId),
 		add_job_on_server(JobId, 
 			Job#job.callback_path, 
 			Job#job.input_path,
 		        Job#job.current_state, 
 			Job#job.current_progress,
 			Job#job.reply_id, 
 			Priority)
 	end,
     mnesia:transaction(F),
    {reply, ok, State};

handle_call({list_jobs}, _From, State) ->
    F = fun() ->
		mnesia:all_keys(job)
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

handle_call({add_task, JobId, TaskType, CallbackPath, InputPath, CurrentState, 
	     Priority}, _From, State) ->
    TaskId = generate_id(),
    add_task_on_server(TaskId, JobId, TaskType, CallbackPath, InputPath, 
	     CurrentState, Priority),
    {reply, TaskId, State};

handle_call({remove_task, TaskId}, _From, State) ->
    remove_task_on_server(TaskId),
    {reply, ok, State};

handle_call({get_task}, _From, State) ->
    F = fun() ->
		MatchHead = #task{task_id = '$1',
 				  job_id = '_',
 				  task_type = '_',
 				  callback_path = '_',
 				  input_path = '_',
 				  current_state = available,
 				  priority = '_'},
 		Result = '$1',
 	        mnesia:select(task, [{MatchHead, [], [Result]}], 1, read)
 	end,
    Result = mnesia:transaction(F),
    case Result of
	{atomic, {[First], _Cont}} ->
	    Task = get_task_info_on_server(First),
	    remove_task_on_server(First),
	    add_task_on_server(Task#task.task_id, 
			       Task#task.job_id, 
			       Task#task.task_type,
			       Task#task.callback_path,
			       Task#task.input_path, 
			       reserved,
			       Task#task.priority),
 	    {reply, Task, State};
 	{atomic, '$end_of_table'} ->
 	    {reply, no_task, State}
    end;

handle_call({get_task_info, TaskId}, _From, State) ->
    Task = get_task_info_on_server(TaskId),
    {reply, Task, State};

handle_call({set_task_state, TaskId, State}, _From, State) ->
    F = fun() ->
		Task = get_task_info_on_server(TaskId),
		remove_task_on_server(TaskId),
		add_task_on_server(TaskId, 
			 Task#task.job_id, 
			 Task#task.task_type,
		         Task#task.callback_path,
			 Task#task.input_path, 
			 State,
			 Task#task.priority)
	end,
    mnesia:transaction(F),
    {reply, ok, State};

handle_call({set_task_priority, TaskId, Priority}, _From, State) ->
    F = fun() ->
		Task = get_task_info_on_server(TaskId),
		remove_task_on_server(TaskId),
		add_task_on_server(TaskId, 
			 Task#task.job_id, 
			 Task#task.task_type,
		         Task#task.callback_path,
			 Task#task.input_path, 
			 Task#task.current_state,
			 Priority)
	end,
    mnesia:transaction(F),
    {reply, ok, State};

handle_call({list_tasks}, _From, State) ->
    F = fun() ->
		mnesia:all_keys(task)
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

handle_call({list_tasks, JobId}, _From, State) ->
    F = fun() ->
		MatchHead = #task_job{task_id = '$1',
				      job_id = '$2'},
 		Result = '$1',
		Guard = {'==', '$2', JobId},
 	        mnesia:select(task_job, [{MatchHead, [Guard], [Result]}])
 	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};
handle_call({list_node_tasks, NodeId}, _From, State) ->
    F = fun() ->
		MatchHead = #task{task_id = '$1',
 				  job_id = '_',
 				  task_type = '_',
 				  callback_path = '_',
 				  input_path = '_',
 				  current_state = {'$2', '$3'},
 				  priority = '_'},
 		Result = '$1',
		Guard1 = {'==', '$2', reserved},
		Guard2 = {'==', '$2', assigned},
		Guard3 = {'==', '$3', NodeId},
 	        mnesia:select(task, [{MatchHead, [Guard1, Guard2, Guard3], 
				      [Result]}], 1, read)
 	end,
    Result = mnesia:transaction(F),
    {reply, Result, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    application:stop(mnesia),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    application:stop(mnesia),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
 
generate_id() ->
    {Megaseconds, Seconds, Microseconds} = now(),
    Id = list_to_integer(integer_to_list(Megaseconds) ++
			    integer_to_list(Seconds) ++
			    integer_to_list(Microseconds)),
    Id.

add_job_on_server(JobId, CallbackPath, InputPath, CurrentState, 
	       CurrentProgress, ReplyId, Priority) ->
    F = fun() ->
		mnesia:write(#job{job_id = JobId,
				  callback_path = CallbackPath,
				  input_path = InputPath,
				  current_state = CurrentState,
				  current_progress = CurrentProgress,
				  reply_id = ReplyId,
				  priority = Priority})
	end,
    mnesia:transaction(F).

remove_job_on_server(JobId) ->
    F = fun() ->
		mnesia:delete({job, JobId})
	end,
    mnesia:transaction(F).

get_job_info_on_server(JobId) ->
    F = fun() ->
		Job = mnesia:read(job, JobId, write),
		Job
	end,
    {atomic, [Result]} = mnesia:transaction(F),
    Result.

add_task_on_server(TaskId, JobId, TaskType, CallbackPath, InputPath, 
	 CurrentState, Priority) ->
    F = fun() ->
		mnesia:write(#task{task_id = TaskId,
				   job_id = JobId,
				   task_type = TaskType,
				   callback_path = CallbackPath,
				   input_path = InputPath,
				   current_state = CurrentState,
				   priority = Priority}),
		mnesia:write(#task_job{task_id = TaskId,
				   job_id = JobId})
	end,
    mnesia:transaction(F),
    TaskId.

remove_task_on_server(TaskId) ->    
    F = fun() ->
		mnesia:delete({task, TaskId}),
		mnesia:delete({task_job, TaskId})
	end,
    mnesia:transaction(F),
    ok.

get_task_info_on_server(TaskId) ->
    F = fun() ->
		mnesia:read(task, TaskId, write)
	end,
    {atomic, [Result]} = mnesia:transaction(F),
    Result.
