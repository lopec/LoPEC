%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <>
%%% @copyright (C) 2009, Clusterbusters
%%% @version 0.0.1
%%% @doc
%%% The erlang process that communicates with the c port driver
%%% on the node.
%%% @end
%%% Created : 30 Sep 2009 by Bjorn Dahlman <>
%%%-------------------------------------------------------------------

-module(computingProcess).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/0, foo/1, bar/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Directory, SharedLib) -> {ok, Pid} |
%%                                              ignore |
%%                                           {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Directory, SharedLib) ->
    case erl_ddll:load_driver(Directory, SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SharedLib], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the process. Will also terminate the port driver
%% associated.
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------

stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Calls the function foo with the argument X in the associated port
%% driver. Returns the return value of the port driver function foo.
%%
%% @spec foo(X::int()) -> int()
%% @end
%%--------------------------------------------------------------------

foo(X) ->
    gen_server:call(?SERVER, {foo, X}).

%%--------------------------------------------------------------------
%% @doc
%% Calls the function bar with the argument X in the associated port
%% driver. Returns the return value of the port driver function bar.
%%
%% @spec bar(X::int()) -> int()
%% @end
%%--------------------------------------------------------------------

bar(Y) ->
    gen_server:call(?SERVER, {bar, Y}).

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
init([SharedLib]) ->
    Port = open_port({spawn, SharedLib}, []),
    {ok, Port}.

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
handle_call(Request, _From, State) ->
    State ! {self(), {command, encode(Request)}},
    receive
	{State, {data, Data}} ->
	    Reply = decode(Data)
    end,
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
handle_cast(_Msg, State) ->
%%    State ! {self(), {command, encode(Request)}},
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
terminate(_Reason, State) ->
    State ! {self(), close},
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

%%--------------------------------------------------------------------
%% @doc
%% Encodes a message according to a C enum, a function name is translated
%% into the corresponding number that it is represented by in the C port.
%%
%% @spec encode({Fn::atom(), Arg::term()}) -> [Fn | [Arg]]
%%      Fn = foo | bar
%%      Argument = term()
%% @end
%%--------------------------------------------------------------------

encode({foo, X}) -> [0, X];
encode({bar, Y}) -> [1, Y].

%%--------------------------------------------------------------------
%% @doc
%% Decodes a message received from the port driver.
%%
%% @spec decode([X::term()]) -> X::term()
%%      X = term()
%% @end
%%--------------------------------------------------------------------

decode([Int]) -> Int.
