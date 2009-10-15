%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Fredrik Andersson
%%% @doc logger holds an API for logging messages on the server.
%%% It uses @see error_logger for info, warning and error messages. Don't use
%%% it for debugging messages, if needed a debugging function can be added to
%%% the API later on. Currently no nice formatting of the message is done it's
%%% simply treated as single whole message and will be printed that way.
%%%
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(chronicler).
-behaviour(gen_server).

-include("../include/chroniclerState.hrl").

%% API
-export([start_link/0,
        error/1,
        info/1,
        warning/1,
	debug/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


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
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%--------------------------------------------------------------------
%% @doc
%% Logs a error message
%%
%% @spec error(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
error(Msg) ->
    gen_server:cast(?MODULE, {error, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Logs a info message
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

%%--------------------------------------------------------------------
%% @doc
%% Logs a debug message
%%
%% @TODO implement using something else than error_logger
%% @spec debug(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
debug(Msg) ->
    gen_server:cast(?MODULE, {info, Msg}).

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
init(no_args) ->
    error_logger:logfile({open, node()}),
    error_logger:tty(true),
    %TODO receive node info from argument
    %TODO state is probably deprecated now, since we use global registering instead
    State = #state{logProcess = ?MODULE, logNode = 'logger@localhost'},
    case "logger" == lists:takewhile(fun(X)->X /= $@ end, atom_to_list(node())) of
        true -> info("I am the externalLogger"),
                global:register_name(externalLoggerPID, self()),
                ok;
        false -> error_logger:add_report_handler(externalLogger, State)
    end,
    info("Chronicle application started"),
    {ok, State}.

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
    {noreply, State}. %no implementation as of now

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling casted messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({error, Msg}, State) ->
    error_logger:error_report(Msg),
    {noreply, State};
handle_cast({error, From, Msg}, State) ->
    error_logger:error_report([From, Msg]),
    {noreply, State};
handle_cast({info, Msg}, State) ->
    error_logger:info_report(Msg),
    {noreply, State};
handle_cast({info, From, Msg}, State) ->
    error_logger:info_report([From, Msg]),
    {noreply, State};
handle_cast({warning, From, Msg}, State) ->
    error_logger:warning_report([From, Msg]),
    {noreply, State};
handle_cast({warning, Msg}, State) ->
    error_logger:warning_report(Msg),
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
    {noreply, State}. %TODO implement if needed

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
terminate(Reason, State) ->
    info({"logger was stopped~n Reason : ~p~n State: ~p~n", Reason, State}),

    %removes the externalHander if we have registered it.
    case "logger" == lists:takewhile(fun(X)->X /= $@ end, atom_to_list(node())) of
        true -> ok;
        false -> error_logger:delete_report_handler(externalLogger)
    end,
    error_logger:logfile(close),
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
