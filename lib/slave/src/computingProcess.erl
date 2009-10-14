%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <>
%%% @copyright (C) 2009, Clusterbusters
%%% @version 0.0.2
%%% @doc
%%% The erlang process that communicates with the external process
%%% on the node.
%%% @end
%%% Created : 30 Sep 2009 by Bjorn Dahlman <>
%%%-------------------------------------------------------------------

-module(computingProcess).

-behaviour(gen_server).

%% API
-export([start_link/4, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(FETCHER, taskFetcher).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server. Path is the path to the external program, Op
%% is the first argument, Arg1 is the second and Arg2 is the third
%% argument. So the os call will look like "Path Op Arg1 Arg2".
%%
%% @spec start_link(Path, Op, Arg1, Arg2) -> {ok, Pid} |
%%                                  ignore |
%%                               {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Path, Op, JobId, InputPath) ->
    StringId = integer_to_list(JobId),
    {ok, Root} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    Prog = Root ++ "programs/" ++ atom_to_list(Path),
    case Op of
	split ->
	    LoadPath = Root ++ "tmp/" ++ StringId ++ "/" ++ InputPath,
	    SavePath = Root ++ "tmp/" ++ StringId ++ "/map/";
	map ->
	    LoadPath = Root ++ "tmp/" ++ StringId ++ "/map/" ++ InputPath,
	    SavePath = Root ++ "tmp/" ++ StringId ++ "/reduce/";
	reduce ->
	    LoadPath = Root ++ "tmp/" ++ StringId ++ "/reduce/" ++ InputPath,
	    SavePath = Root ++ "tmp/" ++ StringId ++ "/results/";
	finalize ->
	    LoadPath = Root ++ "tmp/" ++ StringId ++ "/results/",
	    SavePath = Root ++ "results/" ++ StringId
    end,
    gen_server:start_link({local, ?SERVER},?MODULE,
			  [Prog, atom_to_list(Op), LoadPath, SavePath, JobId], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Path, Op, LoadPath, SavePath, JobId]) ->
    open_port({spawn_executable, Path},
	      [use_stdio, exit_status,
	       {args, [Op, LoadPath, SavePath]}]),
    {ok, JobId}.

%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = no_reply,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    %gen_server:cast(?FETCHER, {self(), halt}),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({_Pid, {data, "NEW_SPLIT " ++ Data}}, State) ->
    chronicler:info(io_lib:format("SPLIT: New map task: ~ts~n", [Data])),
    taskFetcher:new_job(State, "map", Data),
    {noreply, State};
handle_info({_Pid, {data, "NEW_REDUCE_TASK " ++ Data}}, State) ->
    chronicler:info(io_lib:format("MAP: New reduce task: ~ts~n", [Data])),
    taskFetcher:new_job(State, "reduce", Data),
    {noreply, State};
handle_info({_Pid, {data, "NEW_REDUCE_RESULT " ++ Data}}, State) ->
    chronicler:info(io_lib:format("REDUCE: New finalize task: ~ts~n", [Data])),
    taskFetcher:new_job(State, "finalize", Data),
    {noreply, State};
handle_info({_Pid, {data, "ERROR " ++ Data}}, State) ->
    chronicler:info(io_lib:format("ERROR: ~ts~n", [Data])),
    {noreply, State};
handle_info({Pid, {exit_status, Status}}, State) ->
    chronicler:info(io_lib:format("Process ~p exited with signal: ~p~n", [Pid, Status])),
    gen_server:cast(?FETCHER, {self(), done}),
    {stop, normal, State};
handle_info(Data, State) ->
    chronicler:info(io_lib:format("Something: ~ts~n", [Data])),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
