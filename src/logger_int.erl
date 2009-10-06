-module(logger_int).
-export([start/2, stop/1]).

%%%===================================================================
%%% Exported functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Just calls logger_loop with the given arguments.
%% @spec start(ExternalLogger, FlagList::list()) -> term()
%% @todo !!FIXME!!
%% @end
%%--------------------------------------------------------------------
start(ExternalLogger, FlagList) ->
    logger_loop(ExternalLogger, FlagList).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Sends a message to the ExternalLogger and tells it that this
%% logger is shutting down and then stops.
%%
%% @spec stop(ExternalLogger) -> term()
%% @todo !!FIXME!!
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
%% @spec send(ExternalLogger, FlagList, Message) -> term()
%% @todo !!FIXME!!
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
%% @spec logger_loop(ExternalLogger, FlagList::list()) -> term()
%% @todo !!FIXME!!
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
