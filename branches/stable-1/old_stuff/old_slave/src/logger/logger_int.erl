%%%-------------------------------------------------------------------
%%% @author Burbas
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The internal logger
%%% Sends logging messages to the external logger who writes to disk
%%% @end
%%%-------------------------------------------------------------------
-module(logger_int).

%% API
-export([start_link/2, stop/1]).

%% Internal functions
-export([init/2]).

%%%===================================================================
%%% Exported functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Just calls logger_loop with the given arguments.
%% @spec start_link(ExternalLogger, FlagList::list()) ->
%%                                  {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ExternalLogger, FlagList) ->
    Pid = spawn_link(?MODULE, init, [ExternalLogger, FlagList]),
    {ok, Pid}.

init(ExternalLogger, FlagList) ->
    logger_loop(ExternalLogger, FlagList).
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Sends a message to the ExternalLogger and tells it that this
%% logger is shutting down and then stops.
%%
%% @end
%%--------------------------------------------------------------------
stop(ExternalLogger) ->
    ExternalLogger ! {event, self(), "Internal logger is stopping"}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Checks if the current message is of a existing type in the
%% FlagList it sends the message. Else it just discards it.
%%
%% @end
%%--------------------------------------------------------------------
send(_ExternalLogger, [], {_event, _PID, _Type, _Message}) ->
    {ignored};
send(ExternalLogger, [Type|_T], {event, PID, Type, Message}) ->
    ExternalLogger ! {event, PID, Type, Message};
send(ExternalLogger, [_H|T], {event, PID, Type, Message}) ->
    send(ExternalLogger, T, {event, PID, Type, Message}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Waits for messages and relay them to the external logger.
%% If a type is provided it first send the message to the
%% "send(...)" function.
%%
%% @end
%%--------------------------------------------------------------------
logger_loop(ExternalLogger, FlagList) ->
    receive
        {event, PID, Message} ->
            ExternalLogger ! {event, PID, Message},
            logger_loop(ExternalLogger, FlagList);
        {event, PID, Type, Message} ->
            send(ExternalLogger, FlagList, {event, PID, Type, Message}),
            logger_loop(ExternalLogger, FlagList);
        {stop} ->
            stop(ExternalLogger)
    end.
