%%% -------------------------------------------------------------------
%%% Author  : chabbrik
%%% Description :
%%%
%%% Created : Oct 21, 2009
%%% -------------------------------------------------------------------
-module(examiner).
-behaviour(gen_server).
-include("../include/global.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_promising_job/0, get_progress/1, insert/1, remove/1, start_link/0,
         report_created/2, report_assigned/2, report_done/2, report_free/1,
         stop/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

%% @doc
%% Returns the jobid of the job that is closest to be completed.
%%
%% @spec get_promising_job() -> {ok, JobId} | {error, Reason}
%% @end
get_promising_job() ->
    gen_server:call({global, ?MODULE}, {get_promising_job}).

%% @doc
%% Returns the current information about all tasks created by given JobId.
%%
%% @spec get_progress(JobId) ->
%%           {job_stats, JobId,
%%            {FreeSplits, AssignedSplits, DoneSplits},
%%            {FreeMaps, AssignedMaps, DoneMaps},
%%            {FreeReduces, AssignedReduces, DoneReduces},
%%            {FreeFinalizes, AssignedFinalizes, DoneFinalizes}}
%% @end
get_progress(JobId) ->
    gen_server:call({global, ?MODULE}, {get_progress, JobId}).

%% @doc
%% Removes the job with JobId from the examiner.
%%
%% @spec remove(JobId) -> ok
%% @end
remove(JobId) ->
    gen_server:cast({global, ?MODULE}, {remove_entry, JobId}).

%% @doc
%% Report that a task of type TaskType in the job with the id JobId was
%% created.
%%
%% @spec report_created(JobId, TaskType) -> ok
%% @end
report_created(JobId, TaskType) ->
    gen_server:cast({global, ?MODULE},
                    {update_entry, JobId, TaskType, created}).

%% @doc
%% Report that a task of type TaskType in the job with the id JobId was
%% assigned.
%%
%% @spec report_assigned(JobId, TaskType) -> ok
%% @end
report_assigned(JobId, TaskType) ->
    gen_server:cast({global, ?MODULE},
                    {update_entry, JobId, TaskType, assigned}).

%% @doc
%% Report that a task of type TaskType in the job with the id JobId was
%% done.
%%
%% @spec report_done(JobId, TaskType) -> ok
%% @end
report_done(JobId, TaskType) ->
    %gen_server:cast({global, ?MODULE},
    %                {update_entry, JobId, TaskType, done}).
    ok.

%% @doc
%% Report that all tasks ({JobId, TaskType}) in Tasks were freed.
%%
%% @spec report_free(Tasks) -> ok
%% @end
report_free(Tasks) ->
    gen_server:cast({global, ?MODULE},
                    {free_entries, Tasks}).

%% @doc
%% Insert a new job to be tracked by examiner.
%% IMPORTANT: if job is not inserted before usage, it will crash examiner.
%%
%% @spec insert(JobId) -> ok
%% @end
insert(JobId) ->
    gen_server:cast({global, ?MODULE}, {insert_entry, JobId}).

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
%% Stops the server and cleans up.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast({global, ?MODULE}, stop).

%% ====================================================================
%% Server functions
%% ====================================================================

%% @private
init([]) ->
    ets:new(job_status,
            [set, named_table, protected, {keypos, #job_stats.job_id}]),
    {ok, []}.

%% @private
handle_call({get_promising_job}, _From, State) ->
    MostDone =
        fun (Job, '$end_of_table') ->
                Job;
            (JobA, JobB) ->
                JobAProgress = get_progress_percentage(JobA),
                JobBProgress = get_progress_percentage(JobB),
                if JobAProgress > JobBProgress -> JobA;
                   true -> JobB
                end
        end,
    Reply =
        case ets:first(job_status) of
            '$end_of_table' ->
                {error, "There are no jobs."};
            Key ->
                [FirstJob] = ets:lookup(job_status, Key),
                MostDoneJob = ets:foldr(MostDone, FirstJob, job_status),
                {ok, MostDoneJob#job_stats.job_id}
        end,
    {reply, Reply, State};

handle_call({get_progress, JobId}, _From, State) ->
    [Item] = ets:lookup(job_status, JobId),
    {reply, Item, State};

handle_call(Msg, From, State) ->
    chronicler:debug("~w:Received unexpected handle_call from ~p.~n"
                       "Msg: ~p~n",
                       [?MODULE, From, Msg]),
    {noreply, State}.

%% @private
handle_cast({remove_entry, JobId}, State) ->
    ets:delete(job_status, JobId),
    {noreply, State};

handle_cast({update_entry, JobId, TaskType, NewTaskState}, State) ->
    ets:safe_fixtable(job_status,true),
    [Item] = ets:lookup(job_status, JobId),
    case update_job(Item, TaskType, NewTaskState) of
        #job_stats{split = {0,0,_},
                   map = {0,0,_},
                   reduce = {0,0,_},
                   finalize = {0,0,_}} when TaskType == finalize ->
            ets:delete(job_status, JobId);
        UpdatedJob ->
            ets:insert(job_status, UpdatedJob)
    end,
    ets:safe_fixtable(job_status,false),

    {noreply, State};

handle_cast({free_entries, Tasks}, State) ->
    UpdateTask =
        fun ({JobId, TaskType}) ->
                [Job] = ets:lookup(job_status, JobId),
                UpdatedJob = update_job(Job, TaskType, free),
                ets:insert(job_status, UpdatedJob)
        end,
    lists:foreach(fun (TaskEntry) -> UpdateTask(TaskEntry) end, Tasks),
    {noreply, State};

handle_cast({insert_entry, JobId}, State) ->
    NewRecord = #job_stats{job_id = JobId},
    ets:insert(job_status, NewRecord),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    chronicler:debug("~w:Received unexpected handle_cast call.~n"
                       "Info: ~p~n",
                       [?MODULE, Msg]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    chronicler:debug("~w:Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
    {noreply, State}.

%% @private
terminate(Reason, _State) ->
    chronicler:debug("~w:Received terminate call.~n"
                     "Reason: ~p~n",
                     [?MODULE, Reason]),
    ets:delete(job_status),
    ok.

%% @private
code_change(OldVsn, State, Extra) ->
    chronicler:debug("~w:Received unexpected code_change call.~n"
                       "Old version: ~p~n"
                       "Extra: ~p~n",
                       [?MODULE, OldVsn, Extra]),
    {ok, State}.

%% ============================================================
%% Internal functions
%% ============================================================


%% @doc
%% Returns {Event, {NewFree, NewAssigned, NewDone}}, where
%% Event is first_assigned if NewAssigned is 1 and NewDone is 0,
%%          all_done if NewFree and NewAssigned are 0,
%%          nothing otherwise
%% NewFree, NewAssigned and NewDone are updated according to the NewState,
%% which is created, assigned, done, or free.
%%
%% @spec update_task({Free, Assigned, Done}, NewState) ->
%%           {Event, {NewFree, NewAssigned, NewDone}}
%% @end
update_task({Free, Assigned, Done}, NewState) ->
    UpdatedTask =
        case NewState of
            created ->
                {Free + 1, Assigned, Done};
            assigned when Free > 0 ->
                {Free - 1, Assigned + 1, Done};
            done when Assigned > 0 ->
                {Free, Assigned - 1, Done + 1};
            free when Assigned > 0 ->
                {Free + 1, Assigned - 1, Done}
        end,
    Event =
        case UpdatedTask of
            {_, 1, 0} -> first_assigned;
            {0, 0, _} -> all_done;
            {NFree, NAssigned, NDone} when NFree >= 0,
                                           NAssigned >= 0,
                                           NDone >= 0 -> nothing
        end,
    {Event, UpdatedTask}.

%% @doc
%% Returns the {Free, Assigned, Done} tuple of the TaskType in JobStats.
%%
%% @spec get_task(TaskType, JobStats) -> {Free, Assigned, Done}
%% @end
get_task(split, Job) -> Job#job_stats.split;
get_task(map, Job) -> Job#job_stats.map;
get_task(reduce, Job) -> Job#job_stats.reduce;
get_task(finalize, Job) -> Job#job_stats.finalize.

%% @doc
%% Returns JobStats, with the task of TaskType replaced with NewTask.
%%
%% @spec set_task(JobStats, TaskType, NewTask) -> NewJobStats
%% @end
set_task(Job, split, NewTask) -> Job#job_stats{split = NewTask};
set_task(Job, map, NewTask) -> Job#job_stats{map = NewTask};
set_task(Job, reduce, NewTask) -> Job#job_stats{reduce = NewTask};
set_task(Job, finalize, NewTask) -> Job#job_stats{finalize = NewTask}.

%% @doc
%% Returns the integer percentage of total done tasks in JobStats
%%
%% @spec get_progress_percentage(JobStats) -> Percentage
%% @end
get_progress_percentage(Job) ->
    Tasks = [get_task(Type, Job) || Type <- [split, map, reduce, finalize]],
    case lists:foldr(fun ({Free, Assigned, Done}, {TotalAcc, DoneAcc}) ->
                             {TotalAcc + Free + Assigned, DoneAcc + Done}
                     end,
                     {0, 0},
                     Tasks) of
        {Total, _Done} when Total =< 0 ->
            0;
        {Total, Done} ->
            trunc(Done*100/Total)
    end.

%% @doc
%% Returns JobStats with the task state of TaskType updated
%% according to NewTaskState. The user is notified if it's the first assigned,
%% or last done TaskType. The user, db, and statistician are notified if the
%% job is done.
%%
%% @spec update_job(JobStats, TaskType, NewTaskState) ->
%%           UpdatedJob
%% @end
update_job(JobStats, TaskType, NewTaskState) ->
    JobId = JobStats#job_stats.job_id,
    Task = get_task(TaskType, JobStats),
    chronicler:debug("Tuple: ~p", [Task]),
    {Event, UpdatedTask} = update_task(Task, NewTaskState),
    JobName = case listener:get_job_name(JobId) of
                  {name, Name} -> Name;
                  _ -> JobId
              end,
    case Event of
        first_assigned ->
            chronicler:user_info(fix_me_need_user_id,
                "The first ~p task in job ~p was started.",
                                 [TaskType, JobName]);
        all_done ->
            case {all_previous_done(JobStats, TaskType), TaskType} of
                {false, _} -> nothing_to_see_here_move_along;
                {true, finalize} ->
                    chronicler:user_info(fix_me_need_user_id,
                        "Job ~p is done.",
                                         [JobName]),
                    statistician:job_finished(JobId),
                    db:remove_job(JobId);
                {true, _} ->
                    chronicler:user_info(fix_me_need_user_id,
                        "All ~p tasks in job ~p are done.",
                                         [TaskType, JobName])
            end;
        nothing -> ok
    end,
    _UpdatedJob = set_task(JobStats, TaskType, UpdatedTask).

%% @doc
%% true iff all tasks in the previous step of map reduce are done
%%
%% @spec all_previous_done(JobStats, TaskType) -> true | false
%% @end
all_previous_done(#job_stats{split = {0,0,_}},
                  map) ->
    true;
all_previous_done(#job_stats{split = {0,0,_}, map = {0,0,_}},
                  reduce) ->
    true;
all_previous_done(#job_stats{split = {0,0,_}, map = {0,0,_}, reduce = {0,0,_}},
                  finalize) ->
    true;
all_previous_done(_, _) ->
    false.
