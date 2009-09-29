-module(logger_int).
-export([start/1, stop/1]).

% Internal logger


% Starting the internal logger
start(ExternalLogger) ->
    logger_loop(ExternalLogger).

% Stopping the internal logger
stop(ExternalLogger) ->
    ExternalLogger ! {event, self(), "Internal logger is stopping"}.


% The main loop
logger_loop(ExternalLogger) ->
    receive
        {event, PID, Message} ->
           ExternalLogger ! {event, PID, Message},
           logger_loop(ExternalLogger);
        {event, PID, Type, Message} ->
            ExternalLogger ! {event, PID, Type, Message},
            logger_loop(ExternalLogger};
        {stop} ->
            stop(ExternalLogger)
    end.
