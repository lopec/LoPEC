%%%-------------------------------------------------------------------
%%% @author Henkan <henkethalin@hotmail.com>
%%% @author Nordh
%%% @doc
%%%
%%% db.erl contains the database API for the cluster.
%%% The API handles everything the user needs to work the Job and Task tables.
%%%
%%% The database  can also be started in test mode by using db:start(test).
%%% This will only create RAM copies of the db tabels for easy testing.
%%%
%%% @end
%%% Created : 30 Sep 2009 by Henkan
%%%-------------------------------------------------------------------

-module(db).
-include("../include/db.hrl").
-behaviour(gen_server).
-define(SERVER, db_server).

%% APIs for management of the database
-export([start_link/0, start_link/1, stop/0, create_tables/1]).

%% APIs for handling jobs
-export([add_job/1,
         add_bg_job/1,
         remove_job/1,
         set_job_path/2,
         set_job_state/2,
         pause_job/1,
         stop_job/1,
         resume_job/1,
         get_user_jobs/1,
         list_active_jobs/0,
         cancel_job/1,
         increment_task_restarts/1]).

%% APIs for handling tasks
-export([add_task/1,
         fetch_task/1,
         mark_done/1,
         free_tasks/1,
         list/1]).

%% APIs for information
-export([get_job/1,
         get_task/1,
	 get_user_from_job/1]).

%% APIs for users
-export([add_user/3,
	 validate_user/2,
	 get_user/1,
	 set_role/2,
	 set_password/3,
	 set_email/2,
	 exist_user/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% @doc
%%
%% Starts the database gen_server in a test environment, with all tables
%% as ram copies only.
%%
%% @spec start_link(test:atom()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(test) ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    create_tables(ram_copies),
    chronicler:info("~w:Database started in test environment.~n",
                    [?MODULE]),
    {ok, Pid}.

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
    mnesia:create_schema([node()]),
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
    chronicler:info("~w:Database stopped.~n",
                    [?MODULE]),
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%%
%% Creates the tables and the schema used for keeping track of the jobs,
%% tasks and the assigned tasks. When creating the tables in a stable
%% environment, use disc_copies as argument. In test environments
%% ram_copies is preferrably supplied as the argument.
%%
%% @spec create_tables(StorageType::atom()) -> ok
%%                                           | ignore
%%                                           | {error, Error}
%%                  StorageType = ram_copies
%%                              | disc_copies
%%                              | disc_only_copies
%% @end
%%--------------------------------------------------------------------
create_tables(StorageType) ->
    chronicler:info("~w:Tables created.~nType:~p~n",
        [?MODULE, StorageType]),
    gen_server:call(?SERVER, {create_tables, StorageType}).


%%====================================================================
%% Database API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a job to the database. ProgramName is the name of the program
%% to be run, ProblemType is how the problem is run (by default
%% map/reduce for now), Owner is the user who submitted the job and
%% Priority is the priority of the job.
%%
%% @spec add_job({ProgramName::atom(), ProblemType::atom(),
%%                Owner::atom(), Priority::integer()}) ->
%%           JobId | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_job({ProgramName, ProblemType, Owner, Priority}) ->
    _JobId =
        gen_server:call(?SERVER,
                        {add_job,
                         #job{program_name = ProgramName,
                              problem_type = ProblemType,
                              owner        = Owner,
                              priority     = Priority
                             }}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a background job to the database. ProgramName is the name of
%% the program to be run, ProblemType is how the problem is run (by
%% default map/reduce for now), Owner is the user who submitted the
%% job and Priority is the priority of the job.
%%
%% @spec add_bg_job({ProgramName::atom(), ProblemType::atom(),
%%                  Owner::atom(), Priority::integer()}) ->
%%           JobId | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_bg_job({ProgramName, ProblemType, Owner, Priority}) ->
    JobId =
        gen_server:call(?SERVER,
                        {add_job,
                         #job{program_name = ProgramName,
                              problem_type = ProblemType,
                              owner        = Owner,
                              priority     = Priority,
                              is_bg        = true}}),
    gen_server:call(?SERVER, {add_bg_job, JobId}),
    JobId.

%%--------------------------------------------------------------------
%% @doc
%%
%% Removes a job and all its associated tasks.
%%
%% @spec remove_job(JobId) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove_job(JobId) ->
    gen_server:call(?SERVER, {remove_job, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Finds the task which is the next to be worked on and sets it as assigned
%% to the specified node.
%%
%% @spec fetch_task(NodeId::atom()) -> Task::record()
%% @end
%%--------------------------------------------------------------------
fetch_task(NodeId) ->
    gen_server:call(?SERVER, {fetch_task, NodeId}).

%% @private helper for add_task/1
call_add(TableName, JobId, ProgramName, Type, Path) ->
    gen_server:call(?SERVER, {add_task, TableName,
                              #task{job_id = JobId,
                                    program_name = ProgramName,
                                    type = Type,
                                    path = Path}}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a task to the database. The JobId is the id of the job the task
%% belongs to, ProgramName denotes what kind of program the task runs,
%% Type is the task type and Path is the path to the input relative to
%% the NFS root.
%%
%% @spec add_task({JobId::integer(), ProgramName::atom(),
%%                 Type::atom(), Path::atom()}) -> TaskId::integer()
%%           Type = split | map | reduce | finalize
%% @end
%%--------------------------------------------------------------------
add_task({JobId, ProgramName, Type, Path})
  when Type == reduce ; Type == finalize ->
    TableName = list_to_atom(lists:concat([Type, '_free'])),
    case gen_server:call(?SERVER, {exists_path, TableName, JobId, Path}) of
        false ->
            case read(job, JobId) of
                {error, _Reason} ->
                    {error, job_not_in_db};
                _ ->
                    call_add(TableName, JobId, ProgramName, Type, Path)
            end;
        _ ->
            {error, task_not_added}
    end;
add_task({JobId, ProgramName, Type, Path}) when Type == split ; Type == map ->
    TableName = list_to_atom(lists:concat([Type, '_free'])),
    case read(job, JobId) of
        {error, _Reason} ->
            {error, job_not_in_db};
        _ ->
            call_add(TableName, JobId, ProgramName, Type, Path)
    end;
add_task({_JobId, _ProgramName, Type, _Path}) ->
    chronicler:error("~w:add_task failed:~n"
                     "Incorrect input type: ~p~n", [?MODULE, Type]),
    {error, incorrect_input_type}.

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns the whole task from the database given a valid id.
%%
%% @spec get_task(TaskId::integer()) -> Task::record() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_task(TaskId) ->
    gen_server:call(?SERVER, {get_task, TaskId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns the whole job record from the database given a valid id.
%%
%% @spec get_job(JobId::integer()) -> Job::record() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_job(JobId) ->
    gen_server:call(?SERVER, {get_job, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Adds a user to the database.
%%
%% @spec add_user(Username::atom(), Email::string(), Password::string()) 
%%                                 -> {ok, user_added} | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_user(Username, Email, Password) ->
    gen_server:call(?SERVER, {add_user, Username, Email, Password}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Validates a user's name and password to the database.
%%
%% @spec validate_user(Username::atom(), Password::string()) 
%%                                 -> {ok, user_validated} | {error, Error}
%% @end
%%--------------------------------------------------------------------
validate_user(Username, Password) ->
    gen_server:call(?SERVER, {validate_user, Username, Password}).


%%--------------------------------------------------------------------
%% @doc
%%
%% Gets a user from the database.
%%
%% @spec get_user(Username::atom()) -> User | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_user(Username) ->
    gen_server:call(?SERVER, {get_user, Username}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Changes the role (and thus the rights) of a specific user.
%%
%% @spec set_role(Username::atom(), NewRole::atom()) 
%%                                 -> {ok, role_set} | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_role(Username, NewRole) ->
    User = get_user(Username),
    case User of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    NewUser = User#user{role=NewRole},
	    gen_server:call(?SERVER, {modify_user, NewUser}),
	    {ok, role_set}
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Changes the password of a specific user.
%%
%% @spec set_password(Username::atom(), OldPassword::atom(), NewPassword::atom()) 
%%                                 -> {ok, password_set} | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_password(Username, OldPassword, NewPassword) ->
    gen_server:call(?SERVER, {set_password, Username, OldPassword, NewPassword}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Changes the email address of a specific user.
%%
%% @spec set_email(Username::atom(), NewEmail::atom()) 
%%                                 -> {ok, email_set} | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_email(Username, NewEmail) ->
    User = get_user(Username),
    case User of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    NewUser = User#user{email=NewEmail},
	    gen_server:call(?SERVER, {modify_user, NewUser}),
	    {ok, email_set}
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Checks whether a user exists in the database.
%%
%% @spec exist_user(Username::atom()) -> {ok, yes} | {ok, no}
%% @end
%%--------------------------------------------------------------------
exist_user(Username) ->
    case get_user(Username) of
	{error, Reason} ->
	    {ok, no};
	_User ->
	    {ok, yes}
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list of JobIds belonging to the specified user.
%%
%% @spec get_user_jobs(User::atom()) -> List | {error, Error}
%%                       List = [JobId::integer()]
%% @end
%%--------------------------------------------------------------------
get_user_jobs(User) ->
    ListOfJobs = list(job),
    UserJobs = fun(H) ->
                       Job = get_job(H),
                       JobUser = Job#job.owner,
                       case JobUser of
                           User ->
                               true;
                           _ ->
                               false
                       end
               end,
    _Return = lists:filter(UserJobs, ListOfJobs).

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns the user of the given JobId.
%%
%% @spec get_user_from_job(JobId::integer()) -> User::atom() | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_user_from_job(JobId) ->
    Job = get_job(JobId),
    _User = Job#job.owner.

%%--------------------------------------------------------------------
%% @doc
%%
%% Returns a list of all jobs that currently have their states set
%% as 'free'.
%%
%% @spec list_active_jobs() -> List
%%                       List = [JobId::integer()]
%% @end
%%--------------------------------------------------------------------
list_active_jobs() ->
    gen_server:call(?SERVER, {list_active_jobs}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified job.
%%
%% @spec set_job_state(JobId::integer(), NewState::atom()) -> ok
%%                                                          | {error, Error}
%%                               NewState = free | paused | stopped | done
%% @end
%%--------------------------------------------------------------------
set_job_state(JobId, NewState) ->
    gen_server:call(?SERVER, {set_job_state, JobId, NewState}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the path of the specified job.
%%
%% @spec set_job_path(JobId::integer(), NewPath::string()) -> ok
%%                                                          | {error, Error}
%% @end
%%--------------------------------------------------------------------
set_job_path(JobId, NewPath) ->
    gen_server:call(?SERVER, {set_job_path, JobId, NewPath}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified task to done.
%%
%% @spec mark_done(TaskId::integer()) -> ok
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
mark_done(TaskId) ->
    chronicler:debug("~w:Marked task as done.~nTaskId:~p~n",
                     [?MODULE, TaskId]),
    gen_server:call(?SERVER, {set_task_state, TaskId, done}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified job to paused.
%%
%% @spec pause_job(JobId::integer()) -> ok
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
pause_job(JobId) ->
    chronicler:info("~w:Paused job.~nJobId:~p~n",
                    [?MODULE, JobId]),
    set_job_state(JobId, paused).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified job to stopped.
%%
%% @spec stop_job(JobId::integer()) -> TaskList
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop_job(JobId) ->
    gen_server:call(?SERVER, {stop_job, JobId}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified job to stopped.
%% then removes the job from the job tale
%%
%% @spec cancel_job(JobId::integer()) -> TaskList
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
cancel_job(JobId) ->
    NodeList = stop_job(JobId),
    gen_server:call(?SERVER, {remove_job, JobId}),
    chronicler:info("~w:Canceled job.~nJobId:~p~nAffected nodes:~p~n",
                    [?MODULE, JobId, NodeList]),
    NodeList.


%%--------------------------------------------------------------------
%% @doc
%%
%% Set the state of the job to free so it can resume execution.
%%
%% @spec resume_job(JobId::integer()) -> ok
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
resume_job(JobId) ->
    chronicler:info("~w:Resumed job.~nJobId:~p~n",
                    [?MODULE, JobId]),
    set_job_state(JobId, free).

%%--------------------------------------------------------------------
%% @doc
%%
%% Marks all assigned tasks of the specified node as free.
%%
%% @spec free_tasks(NodeId::atom()) -> List
%%                                  | {error, Error}
%%                   List = [{JobId::integer(), TaskType::atom()}]
%% @end
%%--------------------------------------------------------------------
free_tasks(NodeId) ->
    ListOfTasks = list_node_tasks(NodeId),
    Free = fun(H) ->
                   gen_server:call(?SERVER, {set_task_state, H, free})
           end,
    lists:foreach(Free, ListOfTasks),

    MakeReturn = fun(H) ->
                         Task = gen_server:call(?SERVER, {get_task, H}),
                         JobId = Task#task.job_id,
                         TaskType = Task#task.type,
                         {JobId, TaskType}
                 end,
    chronicler:debug("~w:Freed tasks from node:~p~nTasks:~p~n",
                     [?MODULE, NodeId, ListOfTasks]),
    _ReturnList = lists:map(MakeReturn, ListOfTasks).

%%--------------------------------------------------------------------
%% @doc
%%
%% Lists all items in the specified table.
%%
%% @spec list(TableName::atom()) -> List | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(TableName) ->
    gen_server:call(?SERVER, {list, TableName}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all tasks that are assigned to the specified node.
%%
%% @spec list_node_tasks(NodeId::atom()) -> List | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_node_tasks(NodeId) ->
    gen_server:call(?SERVER, {list_node_tasks, NodeId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Increments restart counter in job. If it supercedes the threshold
%% the job is stopped.
%%
%% @spec increment_task_restarts(JobId::integer()) -> NumRestarts::integer()
%% @end
%%--------------------------------------------------------------------
increment_task_restarts(JobId) ->
    gen_server:call(?SERVER, {task_failed, JobId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Starts the mnesia application.
%%
%% @TODO add distribution to several database nodes.
%%
%% @spec init(_Args) -> {ok, NodeList} | {error, Error}
%%         NodeList = [NodeId::atom()]
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    NodeList = [node()],
    application:start(crypto),
    application:start(mnesia),
    {ok, NodeList}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Creates the necessary tables for the database.
%% Should only be called once for initialization if ram_copies is not
%% supplied as the StorageType argument.
%%
%% @spec handle_call({create_tables, StorageType::atom()},
%%                    _From, State) ->
%%                                 {reply, tables_created, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_tables, StorageType}, _From, State) ->
    % Set the options for the tables, such as how to store them.
    Opts = [{type, set}, {StorageType, [node()]}],

    % Create the job tables
    case mnesia:create_table(job,
            [{attributes, record_info(fields, job)}|Opts]) of
        {atomic, ok} ->
            {atomic, ok} =
            mnesia:create_table(bg_job,
                [{attributes, record_info(fields, bg_job)}|Opts]),

            % Create all the task tables
            TaskTableNames =
            [list_to_atom(lists:concat([TaskType, '_', TaskState]))
                || TaskType <- [split, map, reduce, finalize],
                TaskState <- [free, assigned, done]],
            [{atomic, ok} = mnesia:create_table(TableName,
                    [{record_name, task},
                        {attributes,
                            record_info(fields, task)}|Opts])
                || TableName <- TaskTableNames],
            {atomic, ok} =
            mnesia:create_table(assigned_tasks,
                [{record_name, assigned_tasks},
                    {attributes,
                        record_info(fields, assigned_tasks)}|Opts]),
            {atomic, ok} =
            mnesia:create_table(task_relations,
                [{record_name, task_relations},
                    {attributes,
                        record_info(fields, task_relations)}|Opts]),

	    % Create the user table
	    mnesia:create_table(user,
				[{record_name, user},
				 {attributes,
				 record_info(fields, user)}|Opts]),

            % Add secondary keys to some of the tables
            mnesia:add_table_index(assigned_task, job_id),
            mnesia:add_table_index(assigned_task, node_id),
            [mnesia:add_table_index(TableName, job_id)
                || TableName <- TaskTableNames],
            {reply, tables_created, State};
        ERROR ->
            chronicler:debug("?MODULE: Table had already been created "
                             "ERROR: ~w", [ERROR]),
            {reply, {error, tables_existed}, State}
    end;


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a job to the database.
%%
%% @spec handle_call({add_job, ProgramName::atom(), ProblemType::atom(),
%%                    Owner::atom(), Priority::integer()},
%%                    _From, State) ->
%%                                 {reply, JobId::integer(), State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_job, Job}, _From, State) ->
    JobId = generate_id(),
    add(job, Job#job{job_id=JobId}),
    chronicler:debug("~w:Added job.~n"
                     "JobId:~p~n"
                     "Program name:~p~n"
                     "Problem type:~p~n"
                     "Owner:~p~n"
                     "Priority:~p~n",
                     [?MODULE, JobId, Job#job.program_name,
                      Job#job.problem_type, Job#job.owner,
                      Job#job.priority]),
    {reply, JobId, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a job to the database.
%%
%% @spec handle_call({add_bg_job, JobId::integer()}, _From, State) ->
%%           {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_bg_job, JobId}, _From, State) ->
    add(bg_job, #bg_job{job_id=JobId}),
    chronicler:debug("~w:Added bg job.~nJobId:~p~n",
                     [?MODULE, JobId]),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Removes a job and all its associated tasks from the database.
%%
%% @spec handle_call({remove_job, JobId::atom()},
%%                    _From, State) ->
%%                                 {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call({remove_job, JobId}, _From, State) ->
    ListOfTasks = list_job_tasks(JobId),
    remove(job, JobId),
    remove(bg_job, JobId),
    Remove = fun(H) ->
                     Task = read(task_relations, H),
                     remove(Task#task_relations.table_name, H),
                     remove(assigned_tasks, H),
                     remove(task_relations, H)
             end,
    lists:foreach(Remove, ListOfTasks),
    chronicler:debug("~w:Removed job and associated tasks.~nJobId:~p~n",
                     [?MODULE, JobId]),
    {reply, ok, State};

handle_call({stop_job, JobId}, _From, State) ->
    set_job_state_internal(JobId, stopped),
    Reply =
        case get_tasks_assigned_in_job(JobId) of
            {ok, AssignedTasks} ->
                chronicler:user_info("found assigned tasks: ~p", [AssignedTasks]),
                CollectNodes =
                    fun (TaskId, Nodes) ->
                            case read(assigned_tasks, TaskId) of
                                {error, _} -> Nodes;
                                #assigned_tasks{node_id = Node} -> [Node | Nodes]
                            end
                    end,
                ListOfNodes = lists:foldr(CollectNodes, [], AssignedTasks),
                ListForExaminer =
                    [begin TaskRelation = read(task_relations, TaskId),
                           Task = read(TaskRelation#task_relations.table_name, TaskId),
                           {JobId, Task#task.type} end
                     || TaskId <- AssignedTasks],
                examiner:report_free(ListForExaminer),
                lists:foreach(fun (TaskId) -> set_task_state(TaskId, free) end,
                              AssignedTasks),
                chronicler:info("~w:Stopped job.~nJobId:~p~nAffected nodes:~p~n",
                                [?MODULE, JobId, ListOfNodes]),
                ListOfNodes;
            {error, Reason} -> {error, Reason}
        end,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the task which is the next to be worked on and sets it as
%% assigned to NodeId.
%%
%% @spec handle_call({fetch_task, NodeId}, _From, State) ->
%%                                 {reply, Task::record(), State}
%%                               | no_task
%%                               | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_task, NodeId}, _From, State) ->
    %TODO Change this =)
    FetchTask =
        fun(JobId) ->
                Task =
                    case JobId of
                        no_job ->
                            no_task;
                        _ ->
                            case get_split(JobId) of
                                {ok, SplitTask} ->
                                    SplitTask;
                                _ ->
                                    case get_map(JobId) of
                                        {ok, MapTask} ->
                                            MapTask;
                                        _ ->
                                            case get_reduce(JobId) of
                                                {ok, ReduceTask, NodesToKill} ->
                                                    {ReduceTask, NodesToKill};
                                                wait ->
                                                    no_task;
                                                _ ->
                                                    case get_finalize(JobId) of
                                                        {ok, FinalizeTask, NodesToKill} ->
                                                            {FinalizeTask, NodesToKill};
                                                        _ ->
                                                            set_job_state_internal(
                                                              JobId, no_tasks),
                                                            no_task
                                                    end
                                            end
                                    end
                            end
                    end,
                case Task of
                    no_task ->
                        no_task;
                    Task ->
                        TaskId =
                            case Task of
                                {ProperTask, _NodesToKill} ->
                                    ProperTask#task.task_id;
                                ProperTask ->
                                    ProperTask#task.task_id
                            end,
                        set_task_state(TaskId, assigned),
                        add(assigned_tasks,
                            #assigned_tasks{task_id = TaskId,
                                            job_id  = JobId,
                                            node_id = NodeId})
                end,
                Task
        end, % End FetchTask
    GetNonBgJob = fun () -> FetchTask(fetch_job(no_arg)) end,
    GetBgJob = fun () -> FetchTask(fetch_bg_job(no_arg)) end,
    Result =
        case mnesia:transaction(GetNonBgJob) of
            {atomic, no_task} ->
                case mnesia:transaction(GetBgJob) of
                    {atomic, no_task} ->
                        no_task;
                    {atomic, Task} ->
                        case Task of
                            {ProperTask, NodesToKill} ->
                                {ProperTask, NodesToKill};
                            ProperTask ->
                                {ProperTask, []}
                        end
                end;
            {atomic, Task} ->
                case Task of
                    {ProperTask, NodesToKill} ->
                        {ProperTask, NodesToKill};
                    ProperTask ->
                        {ProperTask, []}
                end
        end,
    chronicler:info("~w:Retrieved task.~nTask:~p~n",
                    [?MODULE, Result]),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a task to the database.
%%
%% @spec handle_call({add_task, TableName::atom(), Task},
%%                    _From, State) ->
%%                                 {reply, TaskId::integer(), State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_task, TableName, Task}, _From, State) ->
    TaskId = generate_id(),
    add(TableName, Task#task{task_id = TaskId}),
    TaskRelation =
        #task_relations{task_id    = TaskId,
                        job_id     = Task#task.job_id,
                        table_name = TableName},
    add(task_relations, TaskRelation),
    Reply =
        case read(job, Task#task.job_id) of
            {error, _} ->
                {error, job_not_found};
            Job ->
                case Job#job.state of
                    no_tasks ->
                        set_job_state_internal(Task#task.job_id, free);
                    _ ->
                        ok
                end,

                NodesToKill =
                    case Task#task.type of
                        Type when Type == split ; Type == map ->
                            case Job#job.is_bg of
                                true  -> [];
                                false -> kill_one()
                            end;
                        _ ->
                            []
                    end,

                chronicler:debug("~w:Added task.~nTaskId:~p~n"
                                 "JobId:~p~nType:~p~n",
                                 [?MODULE, TaskId, Task#task.job_id,
                                  Task#task.type]),
                {TaskId, NodesToKill}
        end,
    {reply, Reply, State};


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a whole task given a valid id.
%%
%% @spec handle_call({get_task, TaskId::integer()},
%%                    _From, State) ->
%%                                 {reply, Task, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_task, TaskId}, _From, State) ->
    % First we need to find which table the task is in
    TaskRelation = read(task_relations, TaskId),
    % Then read the task from the correct table
    Task = read(TaskRelation#task_relations.table_name, TaskId),
    {reply, Task, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a whole job given a valid id.
%%
%% @spec handle_call({get_job, JobId::integer()},
%%                    _From, State) ->
%%                                 {reply, Job, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_job, JobId}, _From, State) ->
    Job = read(job, JobId),
    {reply, Job, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the node id of the node which is working on the given task.
%%
%% @spec handle_call({get_node, TaskId::integer()},
%%                    _From, State) ->
%%                                 {reply, NodeId::integer(), State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_node, TaskId}, _From, State) ->
    AssignedTask = read(assigned_tasks, TaskId),
    NodeId = AssignedTask#assigned_tasks.node_id,
    {reply, NodeId, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the state of a task and places the task and its relations in
%% the correct tables.
%%
%% @spec handle_call({set_task_state, TaskId::integer(), NewState::atom()},
%%                    _From, State) ->
%%                                 {reply, Task, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_task_state, TaskId, NewState}, _From, State) ->
    set_task_state(TaskId, NewState),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the state of a job.
%%
%% @spec handle_call({set_job_state, JobId::integer(), NewState::atom()},
%%                    _From, State) ->
%%                                 {reply, ok, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({set_job_state, JobId, NewState}, _From, State) ->
    set_job_state_internal(JobId, NewState),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the path of a job.
%%
%% @spec handle_call({set_job_path, JobId::integer(), NewPath::atom()},
%%                    _From, State) ->
%%                                 {reply, ok, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({set_job_path, JobId, NewPath}, _From, State) ->
    Job = read(job, JobId),
    remove(job, JobId),
    NewJob = Job#job{path = NewPath},
    add(job, NewJob),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Checks to see if a task exists in the given table with the
%% specified JobId and Path.
%%
%% @spec handle_call({exists_path, TableName::atom(), JobId::integer(),
%%                    Path::atom()}, _From, State) ->
%%                                 {reply, true, State}
%%                               | {reply, false, State}
%%                               | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({exists_path, TableName, JobId, Path}, _From, State) ->
    Flag = exists_path(TableName, JobId, Path),
    {reply, Flag, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a user.
%%
%% @spec handle_call({add_user, UserName::atom(), Email::string(),
%%                    Password::string()}, _From, State) ->
%%                                 {reply, {ok, user_added}, State}
%%                               | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({add_user, Username, Email, Password}, _From, State) ->
    PasswordDigest = crypto:sha(Password),
    User = #user{user_name=Username, email=Email, password=PasswordDigest},
    case add(user, User) of
	ok ->
	    Reply = {ok, user_added};
	{error, Reason} ->
	    chronicler:error(
	      "~w:Adding user failed!~nUser: ~s aborted.~nReason: ~s~n", 
	      [?MODULE, User, Reason]),
	    Reply = {error, Reason}
    end,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Validates a user's name and password.
%%
%% @spec handle_call({validate_user, UserName::atom(),
%%                    string::atom()}, _From, State) ->
%%                                 {reply, {ok, user_validated}, State}
%%                               | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({validate_user, Username, Password}, _From, State) ->
    PasswordDigest = crypto:sha(Password),
    User = read(user, Username),
    case User of
	{error, Reason} ->	
	    Reply = {error, Reason};
	_ ->
	    case (User#user.password) of
		PasswordDigest ->
		    Reply = {ok, user_validated};
		_->
		    Reply = {error, invalid}
	    end
    end,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets a user from the database.
%%
%% @spec handle_call({get_user, Username}, _From, State) ->
%%                                 {reply, User, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({get_user, Username}, _From, State) ->
    User = read(user, Username),
    {reply, User, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets the new properties of a user in the database to User.
%%
%% @spec handle_call({modify_user, Username, NewPassword}, _From, State) ->
%%                                 {reply, role_set, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({modify_user, User}, _From, State) ->
    Username = User#user.user_name,
    case read(user, Username) of
	{error, Reason} ->
	    Reply = {error, Reason};
	_ -> 
	    remove(user, Username),
	    add(user, User),
	    Reply = {ok, user_modified}
    end,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sets a new password for a user in the database.
%%
%% @spec handle_call({set_password, Username, OldPassword, NewPassword},
%%                    _From, State) ->
%%                             {reply, password_set, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({set_password, Username, OldPassword, NewPassword}, _From, State) ->
    User = read(user, Username),
    case User of
	{error, Reason} ->
	    Reply = {error, Reason};
	_ ->
	    UserPass = User#user.password,
	    case crypto:sha(OldPassword) of
		UserPass ->
		    remove(user, Username),
		    Password = crypto:sha(NewPassword),
		    add(user, User#user{password=Password}),
		    Reply = {ok, password_set}
	    end
    end,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all items in the specified table.
%%
%% @spec handle_call({list, TableName::atom()}, _From, State) ->
%%                                 {reply, List, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({list, TableName}, _From, State) ->
    F = fun() ->
                mnesia:all_keys(TableName)
        end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all tasks that are assigned to the specified node.
%%
%% @spec handle_call({list_node_tasks, NodeId::atom()}, _From, State) ->
%%                                 {reply, List, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({list_node_tasks, NodeId}, _From, State) ->
    F = fun() ->
                MatchHead = #assigned_tasks{task_id = '$1',
                                            node_id = '$2',
                                            _ = '_'},
                Result = '$1',
                Guard = {'==', '$2', NodeId},
                mnesia:select(assigned_tasks, [{MatchHead, [Guard], [Result]}])
        end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all active jobs.
%%
%% @spec handle_call({list_active_jobs}, _From, State) ->
%%                                 {reply, List, State} | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_call({list_active_jobs}, _From, State) ->
    F = fun() ->
                MatchHead = #job{job_id = '$1',
                                 state = '$2',
                                 _ = '_'},
                Result = '$1',
                Guard = {'==', '$2', free},
                mnesia:select(job, [{MatchHead, [Guard], [Result]}])
        end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Increments restart counter in job. If it supercedes the threshold
%% the job is stopped by dispatcher.
%%
%% @spec handle_call({task_failed, JobId}, _From, State) ->
%%           NumRestarts::integer()
%% @end
%%--------------------------------------------------------------------
handle_call({task_failed, JobId}, _From, State) ->
    F = fun() ->
		Job = read(job, JobId),
		remove(job, JobId),
		NewJob = Job#job{tasks_restarted = Job#job.tasks_restarted + 1},
		add(job, NewJob),
		NewJob#job.tasks_restarted
        end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Stops the mnesia application.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

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
terminate(Reason, _State) ->
    chronicler:debug("~w:Received terminate call.~n"
                     "Reason: ~p~n",
                     [?MODULE, Reason]),
    application:stop(mnesia).
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
    chronicler:warning("~w:Received unexpected code_change call.~n"
                       "Old version: ~p~n"
                       "Extra: ~p~n",
                       [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
    _Id = list_to_integer(lists:concat([Megaseconds, Seconds, Microseconds])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Tries to fetch a free split task from the split_free table, returns no_task
%% if none is found. JobId denotes which job the task should be associated with.
%%
%% @spec get_split(JobId) -> {ok, SplitTask::record()} | no_task
%% @end
%%--------------------------------------------------------------------
get_split(JobId) ->
    SplitTask = find_task_in_table(split_free, JobId),
    case SplitTask of
        no_task -> no_task;
        Split -> {ok, Split}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Tries to fetch a free map task from the map_free table, returns no_task
%% if none is found. JobId denotes which job the task should be associated with.
%%
%% @spec get_map(JobId) -> {ok, MapTask::record()} | no_task
%% @end
%%--------------------------------------------------------------------
get_map(JobId) ->
    MapTask = find_task_in_table(map_free, JobId),
    case MapTask of
        no_task -> no_task;
        Map -> {ok, Map}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Tries to fetch a free reduce task from the reduce_free table, returns no_task
%% if none is found. JobId denotes which job the task should be associated with.
%%
%% @spec get_reduce(JobId) -> {ok, ReduceTask::record()} | no_task
%% @end
%%--------------------------------------------------------------------
get_reduce(JobId) ->
    ReduceTask = find_task_in_table(reduce_free, JobId),
    case ReduceTask of
        no_task ->
            no_task;
        Reduce ->
            SplitAssigned = find_task_in_table(split_assigned, JobId),
            case SplitAssigned of
                no_task ->
                    MapAssigned = find_task_in_table(map_assigned, JobId),
                    case MapAssigned of
                        no_task ->
                            ReduceAssigned =
                                find_task_in_table(reduce_assigned, JobId),
                            case ReduceAssigned of
                                no_task ->
                                    {ok, Reduce, find_nodes_to_kill(JobId, reduce)};
                                _ ->
                                    {ok, Reduce, []}
                            end;
                        _ ->
                            wait
                    end;
                _ ->
                    wait
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Tries to fetch a free finalize task from the finalize_free table, returns
%% no_task if none is found.
%% JobId denotes which job the task should be associated with.
%%
%% @spec get_finalize(JobId) -> {ok, FinalizeTask::record()} | no_task
%% @end
%%--------------------------------------------------------------------
get_finalize(JobId) ->
    FinalizeTask = find_task_in_table(finalize_free, JobId),
    case FinalizeTask of
        no_task ->
            no_task;
        Finalize ->
            ReduceAssigned = find_task_in_table(reduce_assigned, JobId),
            case ReduceAssigned of
                no_task ->
                    FinalizeAssigned =
                        find_task_in_table(finalize_assigned, JobId),
                    case FinalizeAssigned of
                        no_task ->
                            {ok, Finalize, find_nodes_to_kill(JobId, finalize)};
                        _ ->
                            {ok, Finalize, []}
                    end;
                _ ->
                    no_task
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Adds a record to the specified table with name TableName.
%%
%% @spec add(TableName::atom(), Record) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
add(TableName, Record) ->
    F = fun() ->
                mnesia:write(TableName, Record, write)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Removes all records from the given table with the given key.
%%
%% @spec remove(TableName::atom(), Key) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
remove(TableName, Key) ->
    F = fun() ->
                mnesia:delete(TableName, Key, write)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Reads a record from a table.
%%
%% @spec read(TableName::atom(), Key) -> Record | {error, Error}
%% @end
%%--------------------------------------------------------------------
read(TableName, Key) ->
    F = fun() ->
                mnesia:read(TableName, Key)
        end,
    case mnesia:transaction(F) of
        {atomic, [Result]} -> Result;
        {atomic, []} -> {error, key_not_in_table};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Finds the next job to run. _Algorithm is used in case of future
%% implementations using different prioritizing algorithms to choose from.
%%
%% @spec fetch_job(_Algorithm) -> JobId::integer() | {error, Error}
%% @end
%%--------------------------------------------------------------------
fetch_job(_Algorithm) ->
    FindJob =
        fun() ->
                MatchHead =
                    #job{job_id = '$1', state = '$2', is_bg = '$3', _ = '_'},
                FreeGuard = {'==', free, '$2'},
                BgGuard = {'==', false, '$3'},
                Result = '$1',
                mnesia:select(job,
                              [{MatchHead, [FreeGuard, BgGuard], [Result]}],
                              1, read)
        end,

    case mnesia:transaction(FindJob) of
        {atomic, {[First], _Cont}} ->
            First;
        {atomic, '$end_of_table'} ->
            no_job
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Finds the next background job to run. _Algorithm is used in case of
%% future implementations using different prioritizing algorithms to
%% choose from.
%%
%% @spec fetch_bg_job(_Algorithm) -> JobId::integer() | {error, Error}
%% @end
%%--------------------------------------------------------------------
fetch_bg_job(_Algorithm) ->
    FindJob =
        fun() ->
                MatchHead =
                    #job{job_id = '$1', state = '$2', is_bg = '$3', _ = '_'},
                FreeGuard = {'==', free, '$2'},
                BgGuard = {'==', true, '$3'},
                Result = '$1',
                mnesia:select(job,
                              [{MatchHead, [FreeGuard, BgGuard], [Result]}],
                              1, read)
        end,

    case mnesia:transaction(FindJob) of
        {atomic, {[First], _Cont}} ->
            First;
        {atomic, '$end_of_table'} ->
            no_job
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Finds the first free task in the given table associated to JobId.
%%
%% @spec find_task_in_table(TableName::atom(), JobId::integer()) ->
%%                                 Task
%%                               | no_task
%%                               | {error, Error}
%% @end
%%--------------------------------------------------------------------
find_task_in_table(TableName, JobId) ->
    F = fun() ->
                MatchHead = #task{task_id = '$1',
                                  job_id = '$2',
                                  _ = '_'},
                Guard = {'==', '$2', JobId},
                Result = '$1',
                mnesia:select(TableName,
                              [{MatchHead, [Guard], [Result]}], 1, read)
        end,

    Result = mnesia:transaction(F),
    case Result of
        {atomic, {[First], _Cont}} ->
            _Task = read(TableName, First);
        {atomic, '$end_of_table'} ->
            no_task
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified task.
%%
%% @spec set_task_state(TaskId::integer(), NewState::atom()) -> ok
%%                                                            | {error, Error}
%%                               NewState = free | assigned | done
%% @end
%%--------------------------------------------------------------------
set_task_state(TaskId, NewState) ->
    case read(task_relations, TaskId) of
        {error, _} -> {error, task_not_found};
        TaskRelation ->
            TableName = TaskRelation#task_relations.table_name,
            case read(TableName, TaskId) of
                {error, _} -> {error, task_not_found};
                Task ->
                    remove(TableName, TaskId),
                    remove(task_relations, TaskId),
                    remove(assigned_tasks, TaskId),

                    NewTask = Task#task{state = NewState},
                    NewTableName =
                        list_to_atom(lists:concat([Task#task.type,
                                                   '_', NewState])),
                    add(NewTableName, NewTask),
                    add(task_relations,
                        TaskRelation#task_relations{table_name
                                                    = NewTableName}),
                    case NewState of
                        free ->
                            case read(job, Task#task.job_id) of
                                {error, _} ->
                                    {error, job_not_found};
                                Job when Job#job.state /= stopped ->
                                    set_job_state_internal(Task#task.job_id,
                                                           free);
                                _ ->
                                    ok
                            end;
                        done ->
                            JobId = Task#task.job_id,
                            FreeTasks =
                                [find_task_in_table(split_free, JobId),
                                 find_task_in_table(map_free, JobId),
                                 find_task_in_table(reduce_free, JobId),
                                 find_task_in_table(finalize_free, JobId)],
                            F = fun(H) -> H /= no_task end,

                            ExistsTask = lists:any(F, FreeTasks),
                            Job = read(job, JobId),
                            case ExistsTask
                                andalso (Job#job.state == no_tasks) of
                                true ->
                                    set_job_state_internal(JobId, free);
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Sets the state of the specified job.
%%
%% @spec set_job_state_internal(JobId::integer(), NewState::atom()) -> ok
%%                                                            | {error, Error}
%%                               NewState = free | paused | stopped | no_tasks
%% @end
%%--------------------------------------------------------------------
set_job_state_internal(JobId, NewState) ->
    Job = read(job, JobId),
    remove(job, JobId),
    NewJob = Job#job{state = NewState},
    add(job, NewJob).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Lists all tasks that are associated to the specified job.
%%
%% @spec list_job_tasks(JobId::atom()) ->
%%                                 List | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_job_tasks(JobId) ->
    F = fun() ->
                MatchHead = #task_relations{task_id = '$1',
                                            job_id = '$2',
                                            _ = '_'},
                Result = '$1',
                Guard = {'==', '$2', JobId},
                mnesia:select(task_relations, [{MatchHead, [Guard], [Result]}])
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns true if there exists a task in the specified table
%% with job_id JobId and path Path. Otherwise false.
%%
%% @spec exists_path(TableName::atom(), JobId::integer(), Path::list())
%%                                             -> true | false
%% @end
%%--------------------------------------------------------------------
exists_path(TableName, JobId, Path) ->
    F = fun() ->
                mnesia:match_object(TableName,
                                    #task{job_id = JobId, path = Path, _ = '_'},
                                    read)
        end,
    {atomic, Result} = mnesia:transaction(F),
    case Result of
        [] ->
            false;
        List ->
            List
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a list with one node to be killed if there is some assigned
%% task in a bg job on it.
%%
%% @todo Assumes no node is called 'not_killed'
%%
%% @spec kill_one() -> NodesToKill::list()
%% @end
%%--------------------------------------------------------------------
kill_one() ->
    take_bg_nodes(1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a list with nodes assigned to any task in JobId.
%%
%% @spec find_nodes_to_kill(JobId, Tasktype) -> NodesToKill::list()
%% @end
%%--------------------------------------------------------------------
find_nodes_to_kill(JobId, TaskType) ->
    NumFreeTasks = count_free_tasks(JobId, TaskType),
    _NodesToKill = take_bg_nodes(NumFreeTasks).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns the number of free tasks of TaskType in the job JobId
%%
%% @spec count_free_tasks(JobId, Tasktype) -> NumFreeTasks::integer()
%% @end
%%--------------------------------------------------------------------
count_free_tasks(JobId, TaskType) ->
    FreeTable = list_to_atom(lists:concat([TaskType, "_free"])),
    FindTasks =
        fun() ->
                MatchHead =
                    #task{job_id = '$1', task_id = '$2',  _ = '_'},
                Guard = {'==', '$1', JobId},
                Result = '$2',
                mnesia:select(FreeTable, [{MatchHead, [Guard], [Result]}])
        end,
    {atomic, TaskIds} = mnesia:transaction(FindTasks),
    length(TaskIds).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Return a list of nodes assigned to any task in any background jobs.
%% The list has at most NeededNodes elements
%%
%% @spec take_bg_nodes(NeededNodes) -> BgTaskNodes::list()
%% @end
%%--------------------------------------------------------------------
take_bg_nodes(NeededNodes) ->
    ListBgJobs =
        fun () ->
                MatchHead =
                    #job{job_id = '$1', is_bg = true,  _ = '_'},
                Result = '$1',
                mnesia:select(job, [{MatchHead, [], [Result]}])
        end,
    {atomic, JobIds} = mnesia:transaction(ListBgJobs),
    CollectWithIds =
        fun (#assigned_tasks{node_id = NodeId, job_id = JobId},
             Acc) ->
                case lists:member(JobId, JobIds) of
                    true -> [NodeId | Acc];
                    _    -> Acc
                end
        end,
    AssignedNodes =
        case mnesia:transaction(fun () -> mnesia:foldr(CollectWithIds, [], assigned_tasks) end) of
            {atomic, Nodes} -> Nodes;
            _ -> []
        end,
    lists:sublist(AssignedNodes, 1, NeededNodes).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns true iff JobID is in the job table
%%
%% @spec job_exists(JobID) -> bool()
%% @end
%%--------------------------------------------------------------------
job_exists(JobID) ->
    case read(job, JobID) of
        {error, _} -> false;
        _          -> true
    end.

get_tasks_assigned_in_job(JobId) ->
    FindTasks =
        fun() ->
                MatchHead =
                    #assigned_tasks{job_id = '$1', task_id = '$2',  _ = '_'},
                Guard = {'==', '$1', JobId},
                Result = '$2',
                mnesia:select(assigned_tasks, [{MatchHead, [Guard], [Result]}])
        end,

    case mnesia:transaction(FindTasks) of
        {atomic, TaskIds} -> {ok, TaskIds};
        _ -> {error, could_not_find_assigned_tasks}
    end.
