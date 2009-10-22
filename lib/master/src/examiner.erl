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
         start_link/0, update_count/3, update_count/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(job_stats, { job_id,
                     split_free = 0,
                     split_assigned = 0,
                     split_done = 0,
                     map_free = 0,
                     map_assigned = 0,
                     map_done = 0,
                     reduce_free = 0,
                     reduce_assigned = 0,
                     reduce_done = 0,
                     finalize_free = 0,
                     finalize_assigned = 0,
                     finalize_done = 0
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
%%
%% TODO: Add description of update /function_arity
%%
update_count(JobId, TaskType, NewTaskState) -> 
	gen_server:call({global, ?MODULE}, 
                    {update_entry, JobId, TaskType, NewTaskState}).

update_count(JobId, TaskType, OldTaskState, NewTaskState) -> 
    gen_server:call({global, ?MODULE}, 
                    {update_entry, JobId, TaskType, OldTaskState, NewTaskState}).

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
    ets:new(job_status, [set, named_table, protected]),
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
    Key = {TaskType, NewTaskState},
    case Key of
        {split, created} ->
            IncVal = Item#job_stats.split_free + 1,
            NewItem = Item#job_stats{split_free = IncVal},
            ets:insert(job_status, NewItem);
        {split, assigned} ->
            IncVal = Item#job_stats.split_assigned + 1,
            DecVal = Item#job_stats.split_free - 1,
            NewItem = Item#job_stats{split_assigned = IncVal, 
                                     split_free = DecVal},
            ets:insert(job_status, NewItem);
        {split, done} ->
            IncVal = Item#job_stats.split_done + 1,
            DecVal = Item#job_stats.split_assigned - 1,
            NewItem = Item#job_stats{split_done = IncVal, 
                                     split_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {split, available} ->
            IncVal = Item#job_stats.split_free + 1,
            DecVal = Item#job_stats.split_assigned - 1,
            NewItem = Item#job_stats{split_free = IncVal, 
                                     split_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {map, created} ->
            IncVal = Item#job_stats.map_free + 1,
            NewItem = Item#job_stats{map_free = IncVal},
            ets:insert(job_status, NewItem);        
        {map, assigned} ->
            IncVal = Item#job_stats.map_assigned + 1,
            DecVal = Item#job_stats.map_free - 1,
            NewItem = Item#job_stats{map_assigned = IncVal, 
                                     map_free = DecVal},
            ets:insert(job_status, NewItem);
        {map, done} ->
            IncVal = Item#job_stats.split_done + 1,
            DecVal = Item#job_stats.split_assigned - 1,
            NewItem = Item#job_stats{split_done = IncVal, 
                                     split_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {map, available} ->
            IncVal = Item#job_stats.split_free + 1,
            DecVal = Item#job_stats.split_assigned - 1,
            NewItem = Item#job_stats{split_free = IncVal, 
                                     split_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {reduce, created} ->
            IncVal = Item#job_stats.reduce_free + 1,
            NewItem = Item#job_stats{reduce_free = IncVal},
            ets:insert(job_status, NewItem);
        {reduce, assigned} ->
            IncVal = Item#job_stats.reduce_assigned + 1,
            DecVal = Item#job_stats.reduce_free - 1,
            NewItem = Item#job_stats{reduce_assigned = IncVal, 
                                     reduce_free = DecVal},
            ets:insert(job_status, NewItem);
        {reduce, done} ->
            IncVal = Item#job_stats.reduce_done + 1,
            DecVal = Item#job_stats.reduce_assigned - 1,
            NewItem = Item#job_stats{reduce_done = IncVal, 
                                     reduce_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {reduce, available} ->
            IncVal = Item#job_stats.reduce_free + 1,
            DecVal = Item#job_stats.reduce_assigned - 1,
            NewItem = Item#job_stats{reduce_free = IncVal, 
                                     reduce_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {finalize, created} ->
            IncVal = Item#job_stats.finalize_free + 1,
            NewItem = Item#job_stats{finalize_free = IncVal},
            ets:insert(job_status, NewItem);
        {finalize, assigned} ->
            IncVal = Item#job_stats.finalize_assigned + 1,
            DecVal = Item#job_stats.finalize_free - 1,
            NewItem = Item#job_stats{finalize_assigned = IncVal, 
                                     finalize_free = DecVal},
            ets:insert(job_status, NewItem);
        {finalize, done} ->
            IncVal = Item#job_stats.finalize_done + 1,
            DecVal = Item#job_stats.finalize_assigned - 1,
            NewItem = Item#job_stats{finalize_done = IncVal, 
                                     finalize_assigned = DecVal},
            ets:insert(job_status, NewItem);
        {finalize, available} ->
            IncVal = Item#job_stats.finalize_free + 1,
            DecVal = Item#job_stats.finalize_assigned - 1,
            NewItem = Item#job_stats{finalize_free = IncVal, 
                                     finalize_assigned = DecVal},
            ets:insert(job_status, NewItem)
    end,
    {reply, JobId, State};

%%DONE
handle_call({insert_entry, JobId}, _From, State) ->
    NewRecord = #job_stats{job_id = JobId},
    ets:insert(jobs_status, NewRecord),
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
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_cast call.~n"
                         "Info: ~p~n",
                         [?MODULE, Msg])),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_info call.~n"
                         "Info: ~p~n",
                         [?MODULE, Info])),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    chronicler:debug(io_lib:format(
                       "~w:Received terminate call.~n"
                       "Reason: ~p~n",
                       [?MODULE, Reason])),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected code_change call.~n"
                         "Old version: ~p~n"
                         "Extra: ~p~n",
                         [?MODULE, OldVsn, Extra])),
    {ok, State}.