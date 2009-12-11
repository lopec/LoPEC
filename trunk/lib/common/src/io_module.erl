%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <bjorn.dahlman@gmail.com>
%%% @author Vasilij Savin
%%% @copyright (C) 2009, Bjorn Dahlman, Vasilij Savin
%%% @doc
%%% Deals with the temporary storage in the cluster. Gets a binary
%%% stream of data to write or returns the binary stream of data.
%%%
%%% @end
%%% Created :  1 Dec 2009 by Bjorn Dahlman <>
%%%-------------------------------------------------------------------
-module(io_module).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/0, put/3, get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% Currently supported storage types = fs_io_module | riak_io_module
%% @spec start_link(ModuleName::atom(), Args::list()) -> 
%%                  {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ModuleName, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ModuleName, Args], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Puts a value to the storage, either the file system or riak
%% depending on how the server was started.
%%
%% @spec put(Bucket, Key, Val) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
put(Bucket, Key, Val) ->
    gen_server:call(?MODULE, {put, Bucket, Key, Val}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the value associated with the bucket and the key.
%%
%% @spec get(Bucket, Key) -> binary() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get(Bucket, Key) ->
    gen_server:call(?MODULE, {get, Bucket, Key}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server, storage will be Riak if [riak] is passed
%% or the file system if [fs] is passed.
%%
%% @spec init(ModuleAndArgsList) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ModuleName, Args]) ->
    {ok, Conn} = ModuleName:init(Args),
    {ok, [ModuleName, Conn]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. We are not handling any errors here.
%% If operation fails, either we will pass on error message or crash,
%% depending on underlying backend implementation.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call({put, Bucket, Key, Value}, _From, State = [ModuleName, Conn]) ->
    Reply = ModuleName:put(Bucket, Key, Value, Conn),
    {reply, Reply, State};
handle_call({get, Bucket, Key}, _From, State = [ModuleName, Conn]) ->
    Reply = ModuleName:get(Bucket, Key, Conn),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    chronicler:error("Unexpected request: ~p", [Request]),
    {noreply, State}.

%%%===================================================================
%%% Not implemented stuff
%%%===================================================================

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
handle_info(_Info, State) ->
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
