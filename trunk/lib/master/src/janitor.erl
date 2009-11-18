%%%-----------------------------------------------------------------------------
%%% @author Burbas <niclas@burbas.se>
%%% @doc
%%% Our garbagecollector
%%% @end
%%% Created : 12 Okt 2009 by Burbas
%%%-----------------------------------------------------------------------------
-module(janitor).
-include("../include/env.hrl").

-behaviour(gen_server).

-export([
        cleanup_job/1,
        cleanup_split/1,
        cleanup_map/1,
        cleanup_reduce/1,
        cleanup_finalize/1
        ]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc
%% Clean up storage after a job have finished
%%
%% @spec cleanup_job(JobId) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
cleanup_job(JobId) ->
    gen_server:call(?MODULE, {cleanup_job, JobId}).

%%------------------------------------------------------------------------------
%% @doc
%% Clean up storage from split-files belonging to a specific job.
%%
%% @spec cleanup_split(JobId) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
cleanup_split(JobId) ->
    gen_server:call(?MODULE, {cleanup_split, JobId}).

%%------------------------------------------------------------------------------
%% @doc
%% Clean up storage from map-files beloning to a specific job.
%%
%% @spec cleanup_map(JobId) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
cleanup_map(JobId) ->
    gen_server:call(?MODULE, {cleanup_map, JobId}).

%%------------------------------------------------------------------------------
%% @doc
%% Clean up storage from reduce-files beloning to a specific job.
%%
%% @spec cleanup_reduce(JobId) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
cleanup_reduce(JobId) ->
    gen_server:call(?MODULE, {cleanup_reduce, JobId}).

%%------------------------------------------------------------------------------
%% @doc
%% Clean up storage from finalize-files beloning to a specific job.
%%
%% @spec cleanup_finalize(JobId) -> ok | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
cleanup_finalize(JobId) ->
    gen_server:call(?MODULE, {cleanup_finalize, JobId}).

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
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes all tempfiles for a specific job (Removes cluster_root/tmp/JobId)
%%
%% @spec handle_call({cleanup_job, JobId}, From, State) ->
%%           {reply, ok, State} | {reply, {error, Reason}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cleanup_job, JobId}, _From, State) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    JobPath = concat_path([Path, tmp, JobId]),
    ReturnValue = file:del_dir(JobPath),
    {reply, ReturnValue, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes all temporary split-files for a specific job.
%%
%% @spec handle_call({cleanup_split, JobId}, From, State) ->
%%           {reply, ok, State} | {reply, {error, Reason}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cleanup_split, JobId}, _From, State) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    SplitPath = concat_path([Path, tmp, JobId, input]),
    ReturnValue = file:del_dir(SplitPath),
    {reply, ReturnValue, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes all temporary map-files for a specific job.
%%
%% @spec handle_call({cleanup_job, JobId}, From, State) ->
%%           {reply, ok, State} | {reply, {error, Reason}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cleanup_map, JobId}, _From, State) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    MapPath = concat_path([Path, tmp, JobId, map]),
    ReturnValue = file:del_dir(MapPath),
    {reply, ReturnValue, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes all temporary reduce-files for a specific job.
%%
%% @spec handle_call({cleanup_job, JobId}, From, State) ->
%%           {reply, ok, State} | {reply, {error, Reason}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cleanup_reduce, JobId}, _From, State) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    ReducePath = concat_path([Path, tmp, JobId, reduce]),
    ReturnValue = file:del_dir(ReducePath),
    {reply, ReturnValue, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes all temporary finalize-files for a specific job.
%%
%% @spec handle_call({cleanup_job, JobId}, From, State) ->
%%           {reply, ok, State} | {reply, {error, Reason}, State}
%% @end
%%--------------------------------------------------------------------
handle_call({cleanup_finalize, JobId}, _From, State) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    FinalizePath = concat_path([Path, tmp, JobId, results]),
    ReturnValue = file:del_dir(FinalizePath),
    {reply, ReturnValue, State}.

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
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Like lists:concat/1, but adds slashes between items.
%%
%% @spec concat_path(Items :: list()) -> string()
%% @end
%%--------------------------------------------------------------------
concat_path(Items) ->
    concat_path(Items, []).
concat_path([Item], Acc) ->
    lists:concat(lists:reverse([Item | Acc]));
concat_path([Head | Tail], Acc) ->
    concat_path(Tail, ["/", Head | Acc]).
