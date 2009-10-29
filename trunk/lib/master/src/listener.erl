%%%-----------------------------------------------------------------------------
%%% @author Burbas <niclas@burbas.se>
%%% @doc
%%% Listener - The link between our cluster and the user. Process that listens
%%% for new jobs from user. 
%%% @end
%%% Created : 12 Okt 2009 by Burbas
%%%-----------------------------------------------------------------------------
-module(listener).
-include("../include/env.hrl").

-behaviour(gen_server).

-export([add_job/5, add_job/6, pause_job/1, resume_job/1, stop_job/1,
         get_job_name/1, remove_job_name/1, is_valid_jobtype/1]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-record(state, {active_jobs = dict:new()}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} 
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    chronicler:info("~w : module started~n", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% When a new job is reported a series of new directories will be 
%% created and the input file will be moved to this new structure. 
%% When this is done a new split-task is created.
%% Name is the atom the user wants to associate with the job id,
%% or no_name if no association is wanted.
%%
%% @spec add_job(ProgramName, ProblemType, Owner, Priority, InputData, Name) 
%%                  -> JobID
%% @end
%%------------------------------------------------------------------------------
add_job(ProgramName, ProblemType, Owner, Priority, InputData, Name) ->
    chronicler:info("~w : called new_job with a name=~w~n", [?MODULE, Name]),
    gen_server:call(?MODULE, 
       {new_job, ProgramName, ProblemType, Owner, Priority, InputData, Name}).

%% %@spec add_job(ProgramName, ProblemType, Owner, Priority, InputData)
%% %@equiv add_job(JobType, InputData, no_name)
%TODO: Burbas must fix the above two, only he knows how they should look
add_job(ProgramName, ProblemType, Owner, Priority, InputData) ->
    chronicler:info("~w called new_job~n", [?MODULE]),
    add_job(ProgramName, ProblemType, Owner, Priority, InputData, no_name).


pause_job(JobId) ->
    chronicler:user_info("~w : Paused job with Id=~p~n", [?MODULE, JobId]),
    gen_server:call(?MODULE, {pause_job, JobId}).

resume_job(JobId) ->
    chronicler:user_info("~w : Resumed job with Id=~p~n", [?MODULE, JobId]),
    gen_server:call(?MODULE, {resume_job, JobId}).

stop_job(JobId) ->
    chronicler:user_info("~w : Stopped job with Id=~p~n", [?MODULE, JobId]).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the name the user gave JobId when starting it.
%% If no name was given, anonymous is returned
%%
%% @spec get_job_name(JobId) -> {name, Name} | anonymous
%% @end
%%------------------------------------------------------------------------------
get_job_name(JobId) ->
    chronicler:info("~w : called get_job_name with JobId=~B~n", [?MODULE, JobId]),
    gen_server:call(?MODULE, {get_job_name, JobId}).

%%------------------------------------------------------------------------------
%% @doc
%% Disassociates JobId with any saved name.
%%
%% @spec remove_job_name(JobId) -> ok
%% @end
%%------------------------------------------------------------------------------
remove_job_name(JobId) ->
    chronicler:info("~w : called remove_job_name with JobId=~B~n", [?MODULE, JobId]),
    gen_server:call(?MODULE, {remove_job_name, JobId}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @spec init(Args) -> {ok, State} 
%% @end
%%------------------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%           {reply, JobId, State} | {noreply, State}
%% @end
%%------------------------------------------------------------------------------
handle_call({new_job, ProgramName, ProblemType, Owner, Priority, InputData, Name}, 
            _From, State) ->
    JobsWithThisName = dict:filter(fun (_Id, JobName) -> JobName == Name end,
                                   State#state.active_jobs),
    case dict:size(JobsWithThisName) of
        0 ->
            JobId = add_new_job(ProgramName, ProblemType, 
                                Owner, Priority, InputData),
            NewState =
                case Name of
                    no_name -> State;
                    _ ->
                        NewJobs = dict:store(JobId, Name,
                                             State#state.active_jobs),
                        State#state{active_jobs = NewJobs}
                end,
            {reply, {ok, JobId}, NewState};
        _ ->
            Message = "There is already a job with this name, "
                      "pick another one and try again.",
            chronicler:user_info(Message),
            {reply, {error, Message}, State}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the registered name of the JobId, or anonymous
%%
%% @spec handle_call(GetJobName, From, State) ->
%%           {reply, anonymous, State} | {reply, {name, Name}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_job_name, JobId}, _From, State) ->
    Reply = case dict:find(JobId, State#state.active_jobs) of
                error -> anonymous;
                {ok, Name} -> {name, Name}
            end,
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Disassociate any name registered for JobId
%%
%% @spec handle_call(RemoveJobName, From, State) ->
%%           {reply, anonymous, State} | {reply, {name, Name}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({remove_job_name, JobId}, _From, State) ->
    Reply = ok, 
    NewJobs = dict:erase(JobId, State#state.active_jobs),
    NewState = State#state{active_jobs = NewJobs},
    {reply, Reply, NewState};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pauses a job
%%
%% @spec handle_call({pause_job, JobId}, From, State) ->
%%           {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call({pause_job, JobId}, _From, State) ->
    db:pause_job(JobId),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Resumes a job
%%
%% @spec handle_call({resume_job, JobId}, From, State) ->
%%           {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call({resume_job, JobId}, _From, State) ->
    db:resume_job(JobId),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops a job
%%
%% @spec handle_call({stop_job, JobId}, From, State) ->
%%           {reply, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call({stop_job, JobId}, _From, State) ->
    db:stop_job(JobId),
    {reply, ok, State};


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:warning("~w : Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%------------------------------------------------------------------------------
terminate(Reason, _State) ->
    chronicler:info("~w : Received terminate call.~n"
                    "Reason: ~p~n",
                    [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w : Received unexpected handle_cast call.~n"
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
    chronicler:warning("~w : Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
    {noreply, State}.

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
    chronicler:debug("~w : Received unexpected code_change call.~n"
                     "Old version: ~p~n"
                     "Extra: ~p~n",
                     [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if JobType is a valid jobtype. This is done by checking if there exist
%% a "script.sh"-file in the CLUSTERROOT/programs/JobType/
%%
%% @spec is_valid_jobtype(JobType) -> {ok} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
is_valid_jobtype(JobType) ->
    {ok, Root} = 
        configparser:read_config(?CONFIGFILE, cluster_root),
    JobTypeString = atom_to_list(JobType),
    ProgramFile = Root++"programs/"++JobTypeString++"/script.sh",
    % Because there is no function to check if file exists.
    Result = file:rename(ProgramFile, ProgramFile),
    case Result of
        ok -> {ok};
        {error, Reason} -> {error, Reason}
    end.

add_new_job(ProgramName, ProblemType, Owner, Priority, InputData) ->
    case is_valid_jobtype(ProgramName) of
        {ok} ->
           JobId = dispatcher:add_job({ProgramName, ProblemType, Owner, Priority}),
           % Read the structurepath from configfile
            {ok, Root} =
                configparser:read_config(?CONFIGFILE, cluster_root),
            JobRoot = lists:flatten(io_lib:format("/tmp/~p/", [JobId])),
            % Make the directory-structure
            [filelib:ensure_dir(Root ++ JobRoot ++ SubDir)
                || SubDir <- ["map/", "reduce/", "input/", "results/"]],
            % Move the files to the right thing
            Return = file:copy(InputData, Root ++ JobRoot ++ "/input/data.dat"),
	    case Return of
		{ok, BytesCopied} ->
		    chronicler:info(
		      io:format("Split data copied, size: ~p~n",[BytesCopied]));
		{error, Reason} ->
		    chronicler:error(
		      io:format("Could not copy split data, reason: ~p~n",
				[Reason]))
	    end,
            dispatcher:add_task({JobId, ProgramName, split, JobRoot ++ "/input/data.dat"}),
            JobId;
        {error, Reason} ->
            chronicler:info_user("~w : Could not add job. Reason: ~p~n", [?MODULE, Reason]),
            {error, Reason}
    end.
    
