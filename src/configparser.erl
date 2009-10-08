%% Made by Burbas
-module(configparser).
-export([start_link/0, stop/0, init/1, handle_call/3, terminate/2, read_config/2]).
-export([code_change/3, handle_cast/2, handle_info/2, parse/2]).
-behaviour(gen_server).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
   gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the genserver
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).


%%--------------------------------------------------------------------
%% @doc
%% Reads the specified file and gets the value responding to Key
%%
%% @spec read_config(File, Key) -> 
%%                                  {ok, Value} |
%%                                  {error, Reason}
%% @end
%%--------------------------------------------------------------------
read_config(File, Key) ->
    gen_server:call(?MODULE, {read_config, File, Key}).

%%%===================================================================
%%% INTERNAL FUNCTIONS
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Go throu the List and looks if there exist a Key. If so it returns
%% the value of that key.
%%
%% @spec parse(Key, List) -> 
%%                                  {ok, Value} |
%%                                  {error, not_found}
%% @end
%%--------------------------------------------------------------------
parse(_Key, []) ->
  {error, not_found};
parse(Key, [{Key, Value} | _Config]) ->
  {ok, Value};
parse(Key, [{_Other, _Value} | Config]) ->
  parse(Key, Config).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server. 
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, init}.

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
handle_call({read_config, File, Key}, _From, State) ->
    {Ret, Config} = file:consult(File),
    case Ret of 
        error -> {reply, {error, Config}, State};
        ok -> Value = parse(Key, Config),
            {reply, Value, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVersion,State,_Extra) ->
    {ok, State}.

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
handle_cast(_,State) ->
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
terminate(normal,_State) ->
    {ok}.

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
handle_info(_Message,State) ->
    {noreply, State}.
