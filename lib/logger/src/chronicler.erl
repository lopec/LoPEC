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
         debug/1,
         user_info/1,
         error/2,
         info/2,
         warning/2,
         debug/2,
         user_info/2,
         set_logging_level/1
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
%% @spec error(Format, Args) -> ok
%% @equiv error(io_lib:format(Format, Args))
error(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    error(Msg).


%%--------------------------------------------------------------------
%% @doc
%% Logs a info message
%%
%% @spec info(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
info(Msg) ->
    gen_server:cast(?MODULE, {info, Msg}).
%% @spec info(Format, Args) -> ok
%% @equiv info(io_lib:format(Format, Args))
info(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    info(Msg).

%%--------------------------------------------------------------------
%% @doc
%% Logs a warning message
%%
%% @spec warning(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
warning(Msg) ->
    gen_server:cast(?MODULE, {warning, Msg}).
%% @spec warning(Format, Args) -> ok
%% @equiv warning(io_lib:format(Format, Args))
warning(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    warning(Msg).

%%--------------------------------------------------------------------
%% @doc
%% Logs a debug message
%%
%% @TODO implement using something else than error_logger
%% @spec debug(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
debug(Msg) ->
    gen_server:cast(?MODULE, {debug, Msg}).
%% @spec debug(Format, Args) -> ok
%% @equiv debug(io_lib:format(Format, Args))
debug(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    debug(Msg).

%%--------------------------------------------------------------------
%% @doc
%% Logs a user info message
%%
%% @TODO implement using something else than error_logger
%% @spec user_info(Msg) -> ok
%% @end
%%--------------------------------------------------------------------
user_info(Msg) ->
    gen_server:cast(?MODULE, {user_info, Msg}).
%% @spec user_info(Format, Args) -> ok
%% @equiv user_info(io_lib:format(Format, Args))
user_info(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    user_info(Msg).

%%--------------------------------------------------------------------
%% @doc
%% Changes the logging level of the logger, available levels are
%% info, user_info, error, warning and debug
%%
%% @spec set_logging_level(NewLevel) -> ok
%%  NewLevel = list()
%% @end
%%--------------------------------------------------------------------
set_logging_level(NewLevel) ->
    gen_server:cast(?MODULE, {new_level, NewLevel}).


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

    %TODO add module information logging level
    State = #state{loggingLevel = [error, user_info, info, warning, debug]},

    %register the externalLogger if we are not the logger process
    case "logger" == lists:takewhile(fun(X)->X /= $@ end, atom_to_list(node())) of
        true -> info("I am the externalLogger"),
            global:register_name(externalLoggerPID, self()),
            ok;
        false -> error_logger:add_report_handler(externalLogger, State)
    end,

    info("Chronicler application started"),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:warning("~w:Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling casted messages, checks to see if Level is in the logging levels of
%% state
%%
%% @spec handle_cast(Level, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({new_level, NewLevel}, State) ->
    {noreply, State#state{loggingLevel = NewLevel}};
handle_cast({Level, Msg}, State) ->
    case is_level_logging_on(Level, State) of
        true ->
            error_report_message(Level, Msg),
            {noreply, State};
        false ->
            {noreply, State}
    end;
handle_cast({Level, From, Msg}, State) ->
    case is_level_logging_on(Level, State) of
        true ->
            error_report_message(Level, [From, Msg]),
            {noreply, State};
        false ->
            {noreply, State}
    end;
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w:Received unexpected handle_cast call.~n"
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
    chronicler:warning("~w:Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
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
terminate(Reason, State) ->
    info({"logger was stopped~n Reason : ~p~n State: ~p~n", Reason, State}),

    %removes the externalHander if we have registered it.
    case "logger" == lists:takewhile(fun(X)->X /= $@ end, atom_to_list(node())) of
        true -> ok;
        false -> error_logger:delete_report_handler(externalLogger)
    end,

    %close the logfile
    error_logger:logfile(close),
    ok.

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
    chronicler:debug("~w:Received code_change call.~n"
                     "Old version: ~p~n"
                     "Extra: ~p~n",
                     [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks to see if logging is turned on for Level
%%
%% @spec is_level_logging_on(Level, State) -> false | true
%% @end
%%--------------------------------------------------------------------
is_level_logging_on(Level, State) ->
    lists:member(Level, State#state.loggingLevel).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls the correct error_logger reoport function depending on Level.
%%
%% @spec error_report_message(LoggingLevel, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
error_report_message(info, Msg) ->
    error_logger:info_report(Msg);
error_report_message(debug, Msg) ->
    error_logger:info_report(Msg); %TODO Use something else than info_report
error_report_message(user_info, Msg) ->
    error_logger:info_report(Msg); %TODO Use something else than info_report
error_report_message(error, Msg) ->
    error_logger:error_report(Msg);
error_report_message(warning, Msg) ->
    error_logger:warning_report(Msg).
