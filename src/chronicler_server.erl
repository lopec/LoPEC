%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Fredrik Andersson
%%% @doc logger holds an API for logging messages on the server.
%%% @TODO Document more
%%%
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(chronicler_server).
-behaviour(gen_server).

%% API
-export([start_link/1,
         error/1,
         info/1,
         warning/1,
         debug/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {loggerName}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(LoggerName) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LoggerName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, LoggerName, []).

%%--------------------------------------------------------------------
%% @doc
%% Logs a debug message
%%
%% @spec debug(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
debug(Msg) ->
    gen_server:cast(?MODULE, {debug, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Logs an error message
%%
%% @spec error(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
error(Msg) ->
    gen_server:cast(?MODULE, {error, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Logs an info message
%%
%% @spec info(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
info(Msg) ->
    gen_server:cast(?MODULE, {info, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Logs a warning message
%%
%% @spec warning(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
warning(Msg) ->
    gen_server:cast(?MODULE, {warning, Msg}).

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
init(LoggerName) ->
    gen_event:add_handler(LoggerName, terminalLogger, []),
    gen_event:add_handler(LoggerName, fileLogger, [logfile]),
    {ok, #state{loggerName = LoggerName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling casted messages. All messages are handled in the same manner
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    gen_event:notify(State#state.loggerName, Msg),
    {noreply, State}.

%%%===================================================================
%%% Not implemented functionality
%%%===================================================================

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
    {noreply, State}.