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
	 get_task/1, get_task_info/1,
	 get_task_callback_path/1, get_task_input_path/1,
	 get_task_state/1, get_task_priority/1, get_task_type/1, 
	 set_task_state/2, set_task_priority/2,
	 list_tasks/0, list_tasks/1, list_node_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% LOL functions
-export([random/0]).

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts the database gen_server.
%%
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Starts the database gen_server.
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Stops the database gen_server.
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%%
%% Creates the tables used for keeping track of the jobs,
%% tasks and the assigned tasks.
%%
%% @end
%%--------------------------------------------------------------------
create_tables() ->
    gen_server:call(?SERVER, create_tables).


%%====================================================================
%% JOB TABLE APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a job to the job table.
%% 
%% @end
%%--------------------------------------------------------------------
add_job(CallbackPath, InputPath, ReplyId, Priority) ->
    gen_server:call(?SERVER, {add_job, CallbackPath, InputPath,
					ReplyId, Priority}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Removes a job from the job table.
%% 
%% @end
%%--------------------------------------------------------------------
remove_job(JobId) ->
    gen_server:call(?SERVER, {remove_job, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a job from the job table which has its current status set
%% to 'available'.
%% 
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
%% @end
%%--------------------------------------------------------------------
set_job_priority(JobId, Priority) ->
    gen_server:call(?SERVER, {set_job_priority, JobId, Priority}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the current state of the given job with id 'JobId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_job_state(JobId) ->
    Job = get_job_info(JobId),
    Job#job.current_state.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the current progress of the given job with id 'JobId'.
%% 
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
%% @end
%%--------------------------------------------------------------------
get_job_priority(JobId) ->
    Job = get_job_info(JobId),
    Job#job.priority.

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list containing all id's of the jobs in the job table.
%% 
%% @end
%%--------------------------------------------------------------------
list_jobs() ->
    gen_server:call(?SERVER, {list_jobs}).

%%====================================================================
%% TASK TABLE APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a task to the task table with parameters set, and its current
%% state set to 'available'.
%% 
%% @end
%%--------------------------------------------------------------------
add_task(JobId, TaskType, CallbackPath, InputPath, Priority) ->
    gen_server:call(?SERVER, {add_task, JobId, 
					TaskType, CallbackPath, 
					InputPath, available, Priority}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Remove the task with id 'TaskId' from the task table, and its
%% relations from the assigned_task table.
%% 
%% @end
%%--------------------------------------------------------------------
remove_task(TaskId) ->
    gen_server:call(?SERVER, {remove_task, TaskId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns an available task from the task table, and assigns it as
%% reserved to the node with id 'NodeId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task(NodeId) ->
    gen_server:call(?SERVER, {get_task, NodeId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns the whole task with id 'TaskId' from the task table.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_info(TaskId) ->
    gen_server:call(?SERVER, {get_task_info, TaskId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the state of the task with id 'TaskId' to 'State'.
%% 
%% @end
%%--------------------------------------------------------------------
set_task_state(TaskId, State) ->
    gen_server:call(?SERVER, {set_task_state, TaskId, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the priority of the task with id 'TaskId' to 'Priority'.
%% 
%% @end
%%--------------------------------------------------------------------
set_task_priority(TaskId, Priority) ->
    gen_server:call(?SERVER, {set_task_priority, TaskId, Priority}).
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the state of the task with id 'TaskId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_state(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.current_state.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the callback path of the task with id 'TaskId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_callback_path(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.callback_path.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the input path of the task with id 'TaskId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_input_path(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.input_path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the type of the task with id 'TaskId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_type(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.task_type.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the priority of the task with id 'TaskId'.
%% 
%% @end
%%--------------------------------------------------------------------
get_task_priority(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.priority.

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list of id's of all tasks in the task table.
%% 
%% @end
%%--------------------------------------------------------------------
list_tasks() ->
    gen_server:call(?SERVER, {list_tasks}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list of id's of all tasks in the task table which belong
%% to the job with id 'JobId'.
%% 
%% @end
%%--------------------------------------------------------------------
list_tasks(JobId) ->
    gen_server:call(?SERVER, {list_tasks, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list of id's of all tasks in the task table which are
%% reserved or assigned to the node with id 'NodeId'.
%% 
%% @end
%%--------------------------------------------------------------------
list_node_tasks(NodeId) ->
    gen_server:call(?SERVER, {list_node_tasks, NodeId}).
		
%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Creates the mnesia schema, and starts the mnesia application.
%% 
%% @TODO add distribution to several database nodes.
%% 
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    NodeList = [node()],             
    mnesia:create_schema([node()]),  
    application:start(mnesia),
    {ok, NodeList}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Creates the necessary tables for the database to run on disc.
%%
%% @end
%%--------------------------------------------------------------------
handle_call(create_tables, _From, State) ->
    % Set the options for the tables, such as storing them on disc.
    Opts = [{type, set}, {disc_copies, [node()]}],
    {atomic, ok} = mnesia:create_table(
		     job, [{attributes, record_info(fields, job)}|Opts]),
    {atomic, ok} = mnesia:create_table(
		     task, [{attributes, record_info(fields, task)}|Opts]),
    {atomic, ok} = mnesia:create_table(
		     assigned_task, [{attributes, 
				      record_info(fields, assigned_task)}|Opts]),
    {reply, tables_created, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates a unique id for a job, and adds the job to the database on
%% the server.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({add_job,CallbackPath, InputPath, ReplyId, Priority}, 
	    _From, State) ->
    JobId = generate_id(),
    add_job_on_server(JobId, CallbackPath, InputPath, available, undefined,
		   ReplyId, Priority),
    {reply, JobId, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% This handle_call() removes a job from the database. Note that it
%% doesn't remove the associated tasks.
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({remove_job, JobId}, _From, State) ->
    remove_job_on_server(JobId),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Fetches a job from the job table that is marked as "available",
%% and returns the whole record.
%% 
%% @end
%%--------------------------------------------------------------------
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
		% Return one match from the table
	        mnesia:select(job, [{MatchHead, [], [Result]}], 1, read)
	end,

    Result = mnesia:transaction(F),
    case Result of
	% This is what the result of the table match looks like,
	% and we're only interested in the Job.
	{atomic, {[Job], _Cont}} ->
	    % Reply the whole job, not just the id.
 	    {reply, get_job_info_on_server(Job), State};
 	{atomic, '$end_of_table'} ->
 	    {reply, no_job, State}
     end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({get_job_info, JobId}, _From, State) ->
    Job = get_job_info_on_server(JobId),
    {reply, Job, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({list_jobs}, _From, State) ->
    F = fun() ->
		mnesia:all_keys(job)
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({add_task, JobId, TaskType, CallbackPath, InputPath, CurrentState, 
	     Priority}, _From, State) ->
    TaskId = generate_id(),
    add_task_on_server(TaskId, JobId, TaskType, CallbackPath, InputPath, 
	     CurrentState, Priority),
    % Also add the relations the task will have, namely which job and which
    % node (undefined when first inserting the task) it is assigned to.
    add_assigned_task_on_server(TaskId, JobId, undefined),
    {reply, TaskId, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({remove_task, TaskId}, _From, State) ->
    remove_task_on_server(TaskId),
    % Also remove the relations of the task.
    remove_assigned_task_on_server(TaskId),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({get_task, NodeId}, _From, State) ->
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
	    % We need to update the current state of the task
	    % and its relations.
	    Task = get_task_info_on_server(First),
	    remove_task_on_server(First),
	    remove_assigned_task_on_server(First),
	    add_task_on_server(Task#task.task_id, 
			       Task#task.job_id, 
			       Task#task.task_type,
			       Task#task.callback_path,
			       Task#task.input_path, 
			       reserved,
			       Task#task.priority),
	    add_assigned_task_on_server(Task#task.task_id,
					Task#task.job_id,
					NodeId),
 	    {reply, Task, State};
 	{atomic, '$end_of_table'} ->
 	    {reply, no_task, State}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({get_task_info, TaskId}, _From, State) ->
    Task = get_task_info_on_server(TaskId),
    {reply, Task, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({list_tasks}, _From, State) ->
    F = fun() ->
		mnesia:all_keys(task)
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({list_tasks, JobId}, _From, State) ->
    F = fun() ->
		% List the tasks which are connected to
		% the job with id JobId
		MatchHead = #assigned_task{task_id = '$1',
					   job_id = '$2',
					   node_id = '_'},
 		Result = '$1',
		Guard = {'==', '$2', JobId},
 	        mnesia:select(assigned_task, [{MatchHead, [Guard], [Result]}])
 	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({list_node_tasks, NodeId}, _From, State) ->
    F = fun() ->
		MatchHead = #assigned_task{task_id = '$1',
	     				   job_id = '_',
					   node_id = '$2'},
 		Result = '$1',
		Guard = {'==', '$2', NodeId},
 	        mnesia:select(assigned_task, [{MatchHead, [Guard], 
				      [Result]}])
 	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State}.



%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    application:stop(mnesia),
    {stop, normal, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    application:stop(mnesia),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: generate_id() -> Id
%% Description: Generates an id based on now().
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
generate_id() ->
    {Megaseconds, Seconds, Microseconds} = now(),
    Id = list_to_integer(integer_to_list(Megaseconds) ++
			    integer_to_list(Seconds) ++
			    integer_to_list(Microseconds)),
    Id.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
remove_job_on_server(JobId) ->
    F = fun() ->
		mnesia:delete({job, JobId})
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
get_job_info_on_server(JobId) ->
    F = fun() ->
		Job = mnesia:read(job, JobId, write),
		Job
	end,
    {atomic, [Result]} = mnesia:transaction(F),
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
add_task_on_server(TaskId, JobId, TaskType, CallbackPath, InputPath, 
	 CurrentState, Priority) ->
    F = fun() ->
		mnesia:write(#task{task_id = TaskId,
				   job_id = JobId,
				   task_type = TaskType,
				   callback_path = CallbackPath,
				   input_path = InputPath,
				   current_state = CurrentState,
				   priority = Priority})
	end,
    mnesia:transaction(F),
    TaskId.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
remove_task_on_server(TaskId) ->    
    F = fun() ->
		mnesia:delete({task, TaskId})
	end,
    mnesia:transaction(F),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
get_task_info_on_server(TaskId) ->
    F = fun() ->
		mnesia:read(task, TaskId, write)
	end,
    {atomic, [Result]} = mnesia:transaction(F),
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
add_assigned_task_on_server(TaskId, JobId, NodeId) ->
    F = fun() ->
		mnesia:write(#assigned_task{task_id = TaskId,
					    job_id = JobId,
					    node_id = NodeId})
	end,
    mnesia:transaction(F),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
remove_assigned_task_on_server(TaskId) ->
    F = fun() ->
		mnesia:delete({assigned_task, TaskId})
	end,
    mnesia:transaction(F),
    ok.

%%------------------------------------------------------
%% LOL FUNCTIONS
%%------------------------------------------------------
%% random() -> ok
%% This is a really lollerskating function that inserts
%% a lot of things and stuff in the tables. Only to be
%% used for random testing.
random() ->
    A = db:add_job('./1.job', './1.inp', 6643, 10),
    B = db:add_job('./2.job', './2.inp', 56984, 10),
    C = db:add_job('./3.job', './3.inp', 109, 10),
    D = db:add_job('./4.job', './4.inp', 5094, 10),
    E = db:add_job('./5.job', './5.inp', 83567, 10),
    F = db:add_job('./6.job', './6.inp', 4589, 10),
    G = db:add_job('./7.job', './7.inp', 7938, 10),
    H = db:add_job('./8.job', './8.inp', 83567, 10),
    I = db:add_job('./9.job', './9.inp', 7537, 10),
    J = db:add_job('./10.job', './10.inp', 379, 10),
    K = db:add_job('./11.job', './11.inp', 6897, 10),
    L = db:add_job('./12.job', './12.inp', 568, 10),
    M = db:add_job('./13.job', './13.inp', 666, 10),
    
    db:add_task(A, map, './1.tsk', './1.tin', 10),
    db:add_task(A, map, './2.tsk', './2.tin', 10),
    db:add_task(A, map, './3.tsk', './3.tin', 10),
    db:add_task(A, map, './4.tsk', './4.tin', 10),
    db:add_task(A, map, './5.tsk', './5.tin', 10),
    db:add_task(A, map, './6.tsk', './6.tin', 10),
    db:add_task(A, map, './7.tsk', './7.tin', 10),
    db:add_task(A, map, './8.tsk', './8.tin', 10),
    db:add_task(A, map, './9.tsk', './9.tin', 10),
    db:add_task(A, map, './10.tsk', './10.tin', 10),
    db:add_task(A, map, './11.tsk', './11.tin', 10),
    db:add_task(A, map, './12.tsk', './12.tin', 10),
    db:add_task(A, map, './13.tsk', './13.tin', 10),

    db:add_task(B, map, './1.tsk', './1.tin', 10),
    db:add_task(B, map, './2.tsk', './2.tin', 10),
    db:add_task(B, map, './3.tsk', './3.tin', 10),
    db:add_task(B, map, './4.tsk', './4.tin', 10),
    db:add_task(B, map, './5.tsk', './5.tin', 10),
    db:add_task(B, map, './6.tsk', './6.tin', 10),
    db:add_task(B, map, './7.tsk', './7.tin', 10),
    db:add_task(B, map, './8.tsk', './8.tin', 10),
    db:add_task(B, map, './9.tsk', './9.tin', 10),
    db:add_task(B, map, './10.tsk', './10.tin', 10),
    db:add_task(B, map, './11.tsk', './11.tin', 10),
    db:add_task(B, map, './12.tsk', './12.tin', 10),
    db:add_task(B, map, './13.tsk', './13.tin', 10),
    
    db:add_task(C, map, './1.tsk', './1.tin', 10),
    db:add_task(C, map, './2.tsk', './2.tin', 10),
    db:add_task(C, map, './3.tsk', './3.tin', 10),
    db:add_task(C, map, './4.tsk', './4.tin', 10),
    db:add_task(C, map, './5.tsk', './5.tin', 10),
    db:add_task(C, map, './6.tsk', './6.tin', 10),
    db:add_task(C, map, './7.tsk', './7.tin', 10),
    db:add_task(C, map, './8.tsk', './8.tin', 10),
    db:add_task(C, map, './9.tsk', './9.tin', 10),
    db:add_task(C, map, './10.tsk', './10.tin', 10),
    db:add_task(C, map, './11.tsk', './11.tin', 10),
    db:add_task(C, map, './12.tsk', './12.tin', 10),
    db:add_task(C, map, './13.tsk', './13.tin', 10),
    
    db:add_task(D, reduce, './1.tsk', './1.tin', 10),
    db:add_task(D, reduce, './2.tsk', './2.tin', 10),
    db:add_task(D, reduce, './3.tsk', './3.tin', 10),
    db:add_task(D, reduce, './4.tsk', './4.tin', 10),
    db:add_task(D, reduce, './5.tsk', './5.tin', 10),
    db:add_task(D, reduce, './6.tsk', './6.tin', 10),
    db:add_task(D, reduce, './7.tsk', './7.tin', 10),
    db:add_task(D, reduce, './8.tsk', './8.tin', 10),
    db:add_task(D, reduce, './9.tsk', './9.tin', 10),
    db:add_task(D, reduce, './10.tsk', './10.tin', 10),
    db:add_task(D, reduce, './11.tsk', './11.tin', 10),
    db:add_task(D, reduce, './12.tsk', './12.tin', 10),
    db:add_task(D, reduce, './13.tsk', './13.tin', 10),

    db:add_task(E, reduce, './1.tsk', './1.tin', 10),
    db:add_task(E, reduce, './2.tsk', './2.tin', 10),
    db:add_task(E, reduce, './3.tsk', './3.tin', 10),
    db:add_task(E, reduce, './4.tsk', './4.tin', 10),
    db:add_task(E, reduce, './5.tsk', './5.tin', 10),
    db:add_task(E, reduce, './6.tsk', './6.tin', 10),
    db:add_task(E, reduce, './7.tsk', './7.tin', 10),
    db:add_task(E, reduce, './8.tsk', './8.tin', 10),
    db:add_task(E, reduce, './9.tsk', './9.tin', 10),
    db:add_task(E, reduce, './10.tsk', './10.tin', 10),
    db:add_task(E, reduce, './11.tsk', './11.tin', 10),
    db:add_task(E, reduce, './12.tsk', './12.tin', 10),
    db:add_task(E, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(F, reduce, './1.tsk', './1.tin', 10),
    db:add_task(F, reduce, './2.tsk', './2.tin', 10),
    db:add_task(F, reduce, './3.tsk', './3.tin', 10),
    db:add_task(F, reduce, './4.tsk', './4.tin', 10),
    db:add_task(F, reduce, './5.tsk', './5.tin', 10),
    db:add_task(F, reduce, './6.tsk', './6.tin', 10),
    db:add_task(F, reduce, './7.tsk', './7.tin', 10),
    db:add_task(F, reduce, './8.tsk', './8.tin', 10),
    db:add_task(F, reduce, './9.tsk', './9.tin', 10),
    db:add_task(F, reduce, './10.tsk', './10.tin', 10),
    db:add_task(F, reduce, './11.tsk', './11.tin', 10),
    db:add_task(F, reduce, './12.tsk', './12.tin', 10),
    db:add_task(F, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(G, reduce, './1.tsk', './1.tin', 10),
    db:add_task(G, reduce, './2.tsk', './2.tin', 10),
    db:add_task(G, reduce, './3.tsk', './3.tin', 10),
    db:add_task(G, reduce, './4.tsk', './4.tin', 10),
    db:add_task(G, reduce, './5.tsk', './5.tin', 10),
    db:add_task(G, reduce, './6.tsk', './6.tin', 10),
    db:add_task(G, reduce, './7.tsk', './7.tin', 10),
    db:add_task(G, reduce, './8.tsk', './8.tin', 10),
    db:add_task(G, reduce, './9.tsk', './9.tin', 10),
    db:add_task(G, reduce, './10.tsk', './10.tin', 10),
    db:add_task(G, reduce, './11.tsk', './11.tin', 10),
    db:add_task(G, reduce, './12.tsk', './12.tin', 10),
    db:add_task(G, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(H, reduce, './1.tsk', './1.tin', 10),
    db:add_task(H, reduce, './2.tsk', './2.tin', 10),
    db:add_task(H, reduce, './3.tsk', './3.tin', 10),
    db:add_task(H, reduce, './4.tsk', './4.tin', 10),
    db:add_task(H, reduce, './5.tsk', './5.tin', 10),
    db:add_task(H, reduce, './6.tsk', './6.tin', 10),
    db:add_task(H, reduce, './7.tsk', './7.tin', 10),
    db:add_task(H, reduce, './8.tsk', './8.tin', 10),
    db:add_task(H, reduce, './9.tsk', './9.tin', 10),
    db:add_task(H, reduce, './10.tsk', './10.tin', 10),
    db:add_task(H, reduce, './11.tsk', './11.tin', 10),
    db:add_task(H, reduce, './12.tsk', './12.tin', 10),
    db:add_task(H, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(I, reduce, './1.tsk', './1.tin', 10),
    db:add_task(I, reduce, './2.tsk', './2.tin', 10),
    db:add_task(I, reduce, './3.tsk', './3.tin', 10),
    db:add_task(I, reduce, './4.tsk', './4.tin', 10),
    db:add_task(I, reduce, './5.tsk', './5.tin', 10),
    db:add_task(I, reduce, './6.tsk', './6.tin', 10),
    db:add_task(I, reduce, './7.tsk', './7.tin', 10),
    db:add_task(I, reduce, './8.tsk', './8.tin', 10),
    db:add_task(I, reduce, './9.tsk', './9.tin', 10),
    db:add_task(I, reduce, './10.tsk', './10.tin', 10),
    db:add_task(I, reduce, './11.tsk', './11.tin', 10),
    db:add_task(I, reduce, './12.tsk', './12.tin', 10),
    db:add_task(I, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(J, reduce, './1.tsk', './1.tin', 10),
    db:add_task(J, reduce, './2.tsk', './2.tin', 10),
    db:add_task(J, reduce, './3.tsk', './3.tin', 10),
    db:add_task(J, reduce, './4.tsk', './4.tin', 10),
    db:add_task(J, reduce, './5.tsk', './5.tin', 10),
    db:add_task(J, reduce, './6.tsk', './6.tin', 10),
    db:add_task(J, reduce, './7.tsk', './7.tin', 10),
    db:add_task(J, reduce, './8.tsk', './8.tin', 10),
    db:add_task(J, reduce, './9.tsk', './9.tin', 10),
    db:add_task(J, reduce, './10.tsk', './10.tin', 10),
    db:add_task(J, reduce, './11.tsk', './11.tin', 10),
    db:add_task(J, reduce, './12.tsk', './12.tin', 10),
    db:add_task(J, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(K, reduce, './1.tsk', './1.tin', 10),
    db:add_task(K, reduce, './2.tsk', './2.tin', 10),
    db:add_task(K, reduce, './3.tsk', './3.tin', 10),
    db:add_task(K, reduce, './4.tsk', './4.tin', 10),
    db:add_task(K, reduce, './5.tsk', './5.tin', 10),
    db:add_task(K, reduce, './6.tsk', './6.tin', 10),
    db:add_task(K, reduce, './7.tsk', './7.tin', 10),
    db:add_task(K, reduce, './8.tsk', './8.tin', 10),
    db:add_task(K, reduce, './9.tsk', './9.tin', 10),
    db:add_task(K, reduce, './10.tsk', './10.tin', 10),
    db:add_task(K, reduce, './11.tsk', './11.tin', 10),
    db:add_task(K, reduce, './12.tsk', './12.tin', 10),
    db:add_task(K, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(L, reduce, './1.tsk', './1.tin', 10),
    db:add_task(L, reduce, './2.tsk', './2.tin', 10),
    db:add_task(L, reduce, './3.tsk', './3.tin', 10),
    db:add_task(L, reduce, './4.tsk', './4.tin', 10),
    db:add_task(L, reduce, './5.tsk', './5.tin', 10),
    db:add_task(L, reduce, './6.tsk', './6.tin', 10),
    db:add_task(L, reduce, './7.tsk', './7.tin', 10),
    db:add_task(L, reduce, './8.tsk', './8.tin', 10),
    db:add_task(L, reduce, './9.tsk', './9.tin', 10),
    db:add_task(L, reduce, './10.tsk', './10.tin', 10),
    db:add_task(L, reduce, './11.tsk', './11.tin', 10),
    db:add_task(L, reduce, './12.tsk', './12.tin', 10),
    db:add_task(L, reduce, './13.tsk', './13.tin', 10),
    
    db:add_task(M, reduce, './1.tsk', './1.tin', 10),
    db:add_task(M, reduce, './2.tsk', './2.tin', 10),
    db:add_task(M, reduce, './3.tsk', './3.tin', 10),
    db:add_task(M, reduce, './4.tsk', './4.tin', 10),
    db:add_task(M, reduce, './5.tsk', './5.tin', 10),
    db:add_task(M, reduce, './6.tsk', './6.tin', 10),
    db:add_task(M, reduce, './7.tsk', './7.tin', 10),
    db:add_task(M, reduce, './8.tsk', './8.tin', 10),
    db:add_task(M, reduce, './9.tsk', './9.tin', 10),
    db:add_task(M, reduce, './10.tsk', './10.tin', 10),
    db:add_task(M, reduce, './11.tsk', './11.tin', 10),
    db:add_task(M, reduce, './12.tsk', './12.tin', 10),
    db:add_task(M, reduce, './13.tsk', './13.tin', 10),

    db:get_task(busters@node1),
    db:get_task(busters@node1),
    db:get_task(busters@node1),
    db:get_task(busters@node1),
    db:get_task(busters@node1),
    db:get_task(busters@node1),
    db:get_task(busters@node1),

    db:get_task(busters@node2),
    db:get_task(busters@node2),
    db:get_task(busters@node2),
    db:get_task(busters@node2),
    db:get_task(busters@node2),
    db:get_task(busters@node2),
    ok.
