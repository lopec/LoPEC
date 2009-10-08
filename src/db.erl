%%%-------------------------------------------------------------------
%%% @author Henkan
%%% @doc
%%% db.erl is the database API, and provides the necessary
%%% functionality for the user to operate on the job and task tables.
%%%  
%%% For now, when you want to run the database, you start with
%%% typing db:start() in your erlang shell. This will setup the schema
%%% and start the mnesia application. Afterwards, type
%%% db:create:tables() to create the physical tables on the disc.
%%% The tables and stuff will be stored in ./Mnesia.nonode@nohost.
%%% 
%%% Note that you only need to create the tables once, afterwards
%%% you start the database by simply typing db:start(), otherwise
%%% everything blows up.
%%% @end
%%% Created : 30 Sep 2009 by Henkan
%%%-------------------------------------------------------------------

-module(db).
-include("../include/db.hrl").
-behaviour(gen_server).
-define(SERVER, db_server).

%% APIs for management of the databases
-export([start/0, start_link/0, stop/0, create_tables/0,
	 delete_tables/0]).

%% APIs for external access to the job table
-export([add_job/1, remove_job/1, 
	 get_job/0, get_job_info/1, get_job_reply_id/1,
	 get_job_callback_path/1, get_job_input_path/1,
	 get_job_state/1, get_job_priority/1, get_job_progress/1,
	 set_job_state/2, set_job_priority/2, set_job_progress/2,
	 list_jobs/0]).

%% APIs for external access to the task table
-export([add_task/1, remove_task/1,
	 get_task/1, get_task_info/1,
	 get_task_callback_path/1, get_task_input_path/1,
	 get_task_state/1, get_task_priority/1, get_task_type/1, 
	 set_task_state/2, set_task_priority/2, assign_task/2, free_tasks/1,
	 list_tasks/0, list_tasks/1, list_node_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts the database gen_server.
%%
%% @spec start() -> database_started | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link(),
    database_started.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Starts the database server.
%% 
%% @spec start_link() -> {ok,Pid} | ignore | {error, Error} 
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%
%% Stops the database gen_server.
%%
%% @spec stop() -> ok | {error, Error}
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
%% @spec create_tables() -> tables_created | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
create_tables() ->
    gen_server:call(?SERVER, create_tables).

delete_tables() ->
    gen_server:call(?SERVER, delete_tables).

%%====================================================================
%% JOB TABLE APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Calls the server to add a job to the job table with the given
%% properties. The server returns the generated id of the job.
%% The JobType specifies what type of job it is, e.g. ray_tracer, etc.
%% 
%% @spec add_job(
%% { JobType::atom(),
%%   CallbackPath::string(),
%%   InputPath::string(), 
%%   ReplyId::integer(),
%%   Priority::integer()
%% }) -> 
%%   JobId::integer() | {error,Error}
%%   JobType = any() 
%% @end
%%--------------------------------------------------------------------
add_job({JobType, CallbackPath, InputPath, ReplyId, Priority}) ->
    gen_server:call(?SERVER, {add_job, JobType, CallbackPath, InputPath,
			      ReplyId, Priority}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Removes a job from the job table. This does not remove its
%% associated tasks.
%% 
%% @spec remove_job(JobId::integer()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove_job(JobId) ->
    gen_server:call(?SERVER, {remove_job, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a job from the job table which has its current status set
%% to 'available', and sets the status to 'reserved'. 
%% If no such job exists, the atom 'no_job' is returned.
%% 
%% @spec get_job() -> Job | no_job | {error, Error}
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
%% get_job_info(JobId::integer()) -> Job | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_job_info(JobId) ->
    gen_server:call(?SERVER, {get_job_info, JobId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Changes the state of the given job with id 'JobId' to 'State'.
%% The possible states should be either:
%% 'available' - no one is working on the job.
%% 'reserved'  - the job is reserved.
%% 'mapping'   - the job is being mapped.
%% 'reducing'  - the job is being reduced.
%% 'done'      - the job is finished. 
%% 
%% @spec set_job_state(JobId::integer(), State::atom()) -> ok | {error, Error}
%%                 State = available | reserved | mapping | reducing | done
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
%% @spec set_job_progress(JobId::integer(), Progress::integer()) ->
%%                              ok | {error, Error}
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
%% @spec set_job_priority(JobId::integer(), Priority::integer()) ->
%%                              ok | {error, Error}
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
%% @spec get_job_state(JobId::integer()) -> Job | {error, Error}
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
%% @spec get_job_progress(JobId::integer()) -> Progress::integer()
%%                                           | {error, Error}
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
%% @spec get_job_callback_path(JobId::integer()) -> CallbackPath::string()
%%                                                | {error, Error}
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
%% @spec get_job_input_path(JobId::integer()) -> InputPath::string()
%%                                             | {error, Error}
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
%% @spec get_job_reply_id(JobId::integer()) -> ReplyId::string()
%%                                           | {error, Error}
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
%% @spec get_job_priority(JobId::integer()) -> Priority::integer()
%%                                           | {error, Error}
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
%% @spec list_jobs() -> List | {error, Error}
%%              List = [integer()]
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
%% state set to 'available'. The parameter node_id in the
%% assigned_task table is set to 'undefined', as the task is not
%% assigned to any node.
%% 
%% @spec add_task(
%%   { JobId::integer(), 
%%     TaskType::atom(), 
%%     CallbackPath::string(),
%%     InputPath::string(), 
%%     Priority::integer()
%%    }) ->
%%     TaskId::integer() | {error, Error}
%%                 TaskType = mapping | reducing
%% @end
%%--------------------------------------------------------------------
add_task({JobId, TaskType, CallbackPath, InputPath, Priority}) ->
    gen_server:call(?SERVER, {add_task, JobId, 
					TaskType, CallbackPath, 
					InputPath, available, Priority}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Remove the task with id 'TaskId' from the task table, and its
%% relations from the assigned_task table.
%% 
%% @spec remove_task(TaskId::integer()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove_task(TaskId) ->
    gen_server:call(?SERVER, {remove_task, TaskId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns an available task from the task table, and assigns it as
%% reserved to the node with id 'NodeId'. If no such task exists
%% the atom 'no_task' is returned.
%% 
%% @spec get_task(NodeId::atom()) -> Task | no_task | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_task(NodeId) ->
    gen_server:call(?SERVER, {get_task, NodeId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns the whole task with id 'TaskId' from the task table.
%% 
%% @spec get_task_info(TaskId::integer()) -> Task | {error, Error}
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
%% @spec set_task_state(TaskId::integer(), State::atom()) -> ok | {error, Error}
%%                   State = available | reserved | assigned | done
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
%% @spec set_task_priority(TaskId::integer(), Priority::integer()) ->
%%                      ok | {error, Error}
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
%% @spec get_task_state(TaskId::integer()) -> State::atom() | {error, Error}
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
%% @spec get_task_callback_path(TaskId::integer()) -> 
%%                           CallbackPath::string() | {error, Error}
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
%% @spec get_task_input_path(TaskId::integer()) -> 
%%                        InputPath::string() | {error, Error}
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
%% @spec get_task_type(TaskId::integer()) -> TaskType::atom() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_task_type(TaskId) ->
    Task = get_task_info(TaskId),
    Task#task.task_type.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the task as assigned to the specified node.
%%
%% @spec assign_task(TaskId::integer(), NodeId::atom()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
assign_task(TaskId, NodeId) ->
    gen_server:call(?SERVER, {assign_task, TaskId, NodeId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets all tasks that belong to NodeId as available.
%%
%% @spec free_tasks(NodeId::atom()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
free_tasks(NodeId) ->
    ListOfNodeTasks = list_node_tasks(NodeId),
    Free = fun(H) ->
		   assign_task(H, undefined),
		   set_task_state(H, available)
	   end,
    lists:foreach(Free, ListOfNodeTasks),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the priority of the task with id 'TaskId'.
%% 
%% @spec get_task_priority(TaskId::integer()) -> Priority::integer() 
%%                                             | {error, Error}
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
%% @list_tasks() -> List | {error, Error}
%%           List = [TaskId::integer()]
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
%% @spec list_tasks(JobId::integer()) -> List | {error, Error}
%%               List = [TaskId::integer()]
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
%% @spec list_node_tasks(NodeId::integer()) -> List | {error, Error}
%%                    List = [TaskId::integer()]
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
%% @spec init(_Args) -> {ok, NodeList} | {error, Error}
%%         NodeList = [NodeId::atom()]
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
%% Creates the necessary tables for the database on disc.
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


handle_call(delete_tables, _From, State) ->
    F = fun() ->
		mnesia:delete_table(job),
		mnesia:delete_table(task),
		mnesia:delete_table(assigned_task)
	end,
    mnesia:stop(),
    mnesia:transaction(F),
    mnesia:start(),
    {reply, tables_deleted, State};


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates a unique id for a job, and adds the job to the database on
%% the server.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({add_job, JobType, CallbackPath, InputPath, ReplyId, Priority}, 
	    _From, State) ->
    JobId = generate_id(),
    add_job_on_server(JobId, JobType, CallbackPath, InputPath, available, 
		      undefined, ReplyId, Priority),
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
%% Fetches a job from the job table that is marked as 'available',
%% and returns the whole record. Also sets the job as 'reserved'.
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({get_job}, _From, State) ->
    F = fun() ->
		MatchHead = #job{job_id = '$1',
				 job_type = '_',
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
	{atomic, {[JobId], _Cont}} ->
	    Job = get_job_info_on_server(JobId),
	    remove_job_on_server(JobId),
	    add_job_on_server(JobId, 
			      Job#job.job_type,
			      Job#job.callback_path,
			      Job#job.input_path,
			      reserved, 
			      Job#job.current_progress,
			      Job#job.reply_id,
			      Job#job.priority),
	    % Reply the whole job, not just the id.
 	    {reply, get_job_info_on_server(Job), State};
 	{atomic, '$end_of_table'} ->
 	    {reply, no_job, State}
     end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the whole job given the JobId.
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
%% Sets the state of a job.
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({set_job_state, JobId, State}, _From, State) ->
    F = fun() ->
		Job = get_job_info_on_server(JobId),		 
	        remove_job_on_server(JobId),
		add_job_on_server(JobId, 
				  Job#job.job_type,
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
%% Sets the progress of a job.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({set_job_progress, JobId, Progress}, _From, State) ->
     F = fun() ->
 		Job = get_job_info_on_server(JobId),
 		remove_job_on_server(JobId),
 		add_job_on_server(JobId, 
				  Job#job.job_type,
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
%% Sets the priority of a job.
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({set_job_priority, JobId, Priority}, _From, State) ->
     F = fun() ->
 		Job = get_job_info_on_server(JobId),
 		remove_job_on_server(JobId),
 		add_job_on_server(JobId,
				  Job#job.job_type,
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
%% Lists all jobs in the job table.
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
%% Adds a task to the task table and correctly sets its relations.
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
%% Removes the given task and its relations from the database. 
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
%% Returns an available task from the task table and set it as
%% assigned to the specified NodeId. 
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
%% Returns the whole task given a task id.
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
%% Sets the state of a task. 
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({set_task_state, TaskId, NewState}, _From, State) ->
    F = fun() ->
		Task = get_task_info_on_server(TaskId),
		remove_task_on_server(TaskId),
		add_task_on_server(TaskId, 
			 Task#task.job_id, 
			 Task#task.task_type,
		         Task#task.callback_path,
			 Task#task.input_path, 
			 NewState,
			 Task#task.priority)
	end,
    mnesia:transaction(F),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the priority of a task.
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
%% Sets the task as assigned to the given node.
%% 
%% @end
%%--------------------------------------------------------------------
handle_call({assign_task, TaskId, NodeId}, _From, State) ->
    F = fun() ->
		Task = get_task_info_on_server(TaskId),
		remove_assigned_task_on_server(TaskId),
		add_assigned_task_on_server(Task#task.task_id,
					    Task#task.job_id,
					    NodeId)
	end,
    mnesia:transaction(F),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all tasks in the task table.
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
%% Lists all tasks that belong to the job with id JobId.
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
%% Lists all tasks that belong to the node with id NodeId.
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
%% @private
%% @doc
%%
%% Stops the mnesia application.
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
%% Catches all other handle_cast-messages.
%% 
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Handling all non call/cast messages
%% 
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% 
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    application:stop(mnesia).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Convert process state when code is changed
%% 
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates an id based on Erlang's now() function.
%%
%% @spec generate_id() -> Id::integer() 
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
%% Adds a job to the database.
%%
%% add_job_on_server(JobId::integer(), JobType::atom(), CallbackPath::string()
%%                   InputPath::integer(), CurrentState::atom(),
%%                   CurrentProgress::integer(), ReplyId::string(),
%%                   Priority::integer()) -> {aborted, Reason} |{atomic, ok}
%%             JobType = any() 
%%
%% @end
%%--------------------------------------------------------------------
add_job_on_server(JobId, JobType, CallbackPath, InputPath, CurrentState, 
		  CurrentProgress, ReplyId, Priority) ->
    F = fun() ->
		mnesia:write(#job{job_id = JobId,
				  job_type = JobType,
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
%% Removes a job from the server. This does not remove its associated
%% tasks.
%% 
%% @spec remove_job_on_server(JobId::integer()) -> {aborted, Reason} | {atomic, ok}
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
%% Fetches a job given an id from the job table.
%% 
%% @spec get_job_info_on_server(JobId::integer()) -> Job
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
%% Adds a task to the task table.
%% 
%% @spec add_task_on_server(TaskId::integer(), JobId::integer(), 
%%                          TaskType::atom(), CallbackPath::string(), 
%%                          InputPath::string(), CurrentState::atom(),
%%                          Priority::integer()) -> 
%%                      {aborted, Reason} | ok
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
%% Removes a task from the task table.
%% 
%% @spec remove_task_on_server(TaskId::integer()) -> ok | {error, Error}
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
%% Fetches a task from the task table given an id.
%% 
%% @spec get_task_info_on_server(TaskId::integer()) -> Task
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
%% Add the relations of a task to the assigned_task table.
%% 
%% @spec add_assigned_task_on_server(TaskId::integer(), JobId::integer(),
%%                                   NodeId::integer()) ->
%%                       ok | {error, Error}
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
%% Removes the relations of a given task from the assigned_task table.
%% 
%% @spec remove_assigned_task_on_server(TaskId::integer()) -> 
%%                           ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove_assigned_task_on_server(TaskId) ->
    F = fun() ->
		mnesia:delete({assigned_task, TaskId})
	end,
    mnesia:transaction(F),
    ok.
