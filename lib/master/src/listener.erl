%%%-----------------------------------------------------------------------------
%%% @author Burbas
%%% @doc
%%% Listener - The link between our cluster and the user
%%% @end
%%% Created : 12 Okt 2009 by Burbas
%%%-----------------------------------------------------------------------------
-module(listener).
-behaviour(gen_server).

-export([start_link/0, new_job/2, is_valid_jobtype/1]).
-export([init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% When a new job is reported a series of new directorys will be 
%% created and the input file will be moved to this new structure. 
%% When this is done a new split-task is created.
%%
%% @end
%%------------------------------------------------------------------------------
new_job(TaskType, InputData) ->
    gen_server:call(?MODULE, {new_job, TaskType, InputData}).

%%------------------------------------------------------------------------------
%% @doc
%% Checks if JobType is a valid jobtype. This is done by checking if there exist
%% a "script.sh"-file in the CLUSTERROOT/programs/JobType/
%%
%% @spec is_valid_jobtype(JobType) -> {ok} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
is_valid_jobtype(JobType) ->
    {ok, Root} = 
        config_parser:read_config("/etc/clusterbusters.conf", cluster_root),
    JobTypeString = atom_to_list(JobType),
    ProgramFile = Root++"programs/"++JobTypeString++"/script.sh",
    Result = file:rename(ProgramFile, ProgramFile),
    case Result of
        ok -> {ok};
        {error, Reason} -> {error, Reason}
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%------------------------------------------------------------------------------
init(_Args) ->
    {ok, []}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_call({stop}, _From, State) ->
    {ok, normal, State};
handle_call({new_job, JobType, InputData}, _From, State) ->
    % Call the taskadder
    {JobId} = dispatcher:add_job({JobType, 0}),
    % Read the structurepath from configfile
    {ok, Root} = 
        config_parser:read_config("/etc/clusterbusters.conf", cluster_root),
    % Make the directory-structure
    JobIdString = lists:flatten(io_lib:format("~p", [JobId])),
    filelib:ensure(Root ++ "tmp/" ++ JobIdString ++ "/map"),
    filelib:ensure(Root ++ "tmp/" ++ JobIdString ++ "/reduce"),
    filelib:ensure(Root ++ "tmp/" ++ JobIdString ++ "/input"), 
    % Move the files to the right thing
    file:rename(InputData, Root ++ "tmp/" ++ JobIdString ++ "/input/data.dat"),
    % Call the taskadder with new information
    dispatcher:add_task({JobId, split, Root++"tmp/"++JobIdString++"/input/data.dat"}),
    {reply, created_job, State}.

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
terminate(normal, _State) ->
    {ok}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVersion,State, _Extra) ->
    {ok, State}.
