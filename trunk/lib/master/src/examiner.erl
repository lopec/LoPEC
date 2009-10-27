%%% -------------------------------------------------------------------
%%% Author  : chabbrik
%%% Description :
%%%
%%% Created : Oct 21, 2009
%%% -------------------------------------------------------------------
-module(examiner).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([get_promising_job/0, get_progress/1, insert/1, remove/1, 
         start_link/0, update_count/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(job_stats, { job_id,
                     split = {0, 0, 0},
                     map = {0, 0, 0},
                     reduce = {0, 0, 0},
                     finalize = {0, 0, 0}
                    }).
%% ====================================================================
%% External functions
%% ====================================================================
%%
%% TODO: Add description of get_promising_job/function_arity
%%
get_promising_job() ->
    gen_server:call({global, ?MODULE}, {get_job}).
%%
%% TODO: Add description of get_progress/function_arity
%%
get_progress(JobId) ->
    gen_server:call({global, ?MODULE}, {get_progress, JobId}).

%%
%% TODO: Add description of remove/function_arity
%%
remove(JobId) ->
	gen_server:call({global, ?MODULE}, {remove_entry, JobId}).

%% @doc
%% Updates progress meter according to latest changes.
%% JobId - job associated with task.
%% TaskType - what type of task was modified (map, reduce, split, finalize)
%% NewTaskState - state task changes to (free, assigned, done)
%% @end
update_count(JobId, TaskType, NewTaskState) ->
    gen_server:call({global, ?MODULE}, 
                    {update_entry, JobId, TaskType, NewTaskState}).

%%
%% TODO: Add description of update/function_arity
%%
insert(JobId) ->
    gen_server:call({global, ?MODULE}, {insert_entry, JobId}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ets:new(job_status,
            [set, named_table, protected, {keypos, #job_stats.job_id}]),
    {ok, []}.

%% --------------------------------------------------------------------
%% @doc
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%% @end
%% --------------------------------------------------------------------
handle_call({get_job}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

%%DONE
handle_call({get_progress, JobId}, _From, State) ->
    [Item] = ets:lookup(job_status, JobId),
    {reply, Item, State};

%%DONE
handle_call({remove_entry, JobId}, _From, State) ->
    ets:delete(job_status, JobId),
    {reply, JobId, State};

% IF empty list - error, let it blow up!
handle_call({update_entry, JobId, TaskType, NewTaskState},
             _From, State) ->
    [Item] = ets:lookup(job_status, JobId),
    Task = get_task(TaskType, Item),
    {Event, UpdatedTask} = update_task(Task, NewTaskState),
    case Event of
        first_assigned ->
            chronicler:user_info("The first ~p task in ~p was started.",
                                 [TaskType, JobId]);
        all_done when TaskType == finalize ->
            chronicler:user_info("The job ~p is done.",
                                 [JobId]);
        all_done ->
            chronicler:user_info("All ~p tasks in ~p are done.",
                                 [TaskType, JobId]);
        nothing -> ok
    end,
    UpdatedJob = set_task(Item, TaskType, UpdatedTask),
    ets:insert(job_status, UpdatedJob),
    {reply, JobId, State};

%%DONE
handle_call({insert_entry, JobId}, _From, State) ->
    NewRecord = #job_stats{job_id = JobId},
    ets:insert(job_status, NewRecord),
    {reply, JobId, State}.

%% --------------------------------------------------------------------
%%% Not implemented functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w:Received unexpected handle_cast call.~n"
                       "Info: ~p~n",
                       [?MODULE, Msg]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    chronicler:warning("~w:Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    chronicler:debug("~w:Received terminate call.~n"
                     "Reason: ~p~n",
                     [?MODULE, Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    chronicler:warning("~w:Received unexpected code_change call.~n"
                       "Old version: ~p~n"
                       "Extra: ~p~n",
                       [?MODULE, OldVsn, Extra]),
    {ok, State}.

%% ============================================================
%% Internal functions
%% ============================================================

update_task({Free, Assigned, Done}, NewState) ->
    UpdatedTask =
        case NewState of
            created ->
                {Free + 1, Assigned, Done};
            assigned ->
                {Free - 1, Assigned + 1, Done};
            done ->
                {Free, Assigned - 1, Done + 1};
            available ->
                {Free + 1, Assigned - 1, Done}
        end,
    Event =
        case UpdatedTask of
            {_, 1, 0} -> first_assigned;
            {0, 0, _} -> all_done;
            _ -> nothing
        end,
    {Event, UpdatedTask}.

get_task(split, Job) -> Job#job_stats.split;
get_task(map, Job) -> Job#job_stats.map;
get_task(reduce, Job) -> Job#job_stats.reduce;
get_task(finalize, Job) -> Job#job_stats.finalize.

set_task(Job, split, NewTask) -> Job#job_stats{split = NewTask};
set_task(Job, map, NewTask) -> Job#job_stats{map = NewTask};
set_task(Job, reduce, NewTask) -> Job#job_stats{reduce = NewTask};
set_task(Job, finalize, NewTask) -> Job#job_stats{finalize = NewTask}.

get_progress_percentage(Job) ->
    Tasks = [get_task(Type, Job) || Type <- [split, map, reduce, finalize]],
    {Total, Done} =
        lists:foldr(fun ({Free, Assigned, Done}, {TotalAcc, DoneAcc}) ->
                            {TotalAcc + Free + Assigned, DoneAcc + Done}
                    end,
                    {0, 0},
                    Tasks),
    trunc(Done*100/Total).
