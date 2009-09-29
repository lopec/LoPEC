-module(logger_int).
-export([start_link/2, init/2, stop/1]).

% Internal logger

% Starting the internal logger
start_link(ExternalLogger, FlagList) ->
    Pid = spawn_link(?MODULE, init, [ExternalLogger, FlagList]),
    {ok, Pid}.

init(ExternalLogger, FlagList) ->
    logger_loop(ExternalLogger, FlagList).

% Stopping the internal logger
stop(ExternalLogger) ->
    ExternalLogger ! {event, self(), "Internal logger is stopping"}.

% Determines if the message with type should be sent. 
% If "Type" is in FlagList then it will be sent, else not.
send(_ExternalLogger, [], {_event, _PID, _Type, _Message}) ->
    {ignored};
send(ExternalLogger, [Type|_T], {event, PID, Type, Message}) ->
    ExternalLogger ! {event, PID, Type, Message};
send(ExternalLogger, [_H|T], {event, PID, Type, Message}) ->
    send(ExternalLogger, T, {event, PID, Type, Message}).

% The main loop
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
