-module(logger_ext).
-export([start/1]).

% A very simple logger (External)
% ====================
% Maybe we should have different log-types (Error, Warning, Info)?


% Starts the server and opens a file named "Filename" 
% and calls logger_loop-function
start(Filename) ->
	{ok, IoDevice} = file:open(Filename, [raw,append]),
	file:write(IoDevice, "Starting logger for JobID: "++Filename++"\n"),
	logger_loop(IoDevice).


% Writes a "Stopping"-message to the file and then closes it.
stop(IoDevice) ->
	file:write(IoDevice, "Stopping logger\n"),
	file:close(IoDevice).

% Main loop. Waits for messages on form:
	% {event, PID, Message}
% and prints it to a filehandler "IoDevice".
logger_loop(IoDevice) ->
	receive 
		{event, PID, Message} -> 
			Time = dh_date:format("H:i:s", erlang:localtime()),
			file:write(IoDevice, ("[PID] "++Time++" - "++Message++"\n")),
			logger_loop(IoDevice);
        {event, PID, Type, Message} ->
            Time = dh_date:format("H:i:s", erlang:localtime()),
            file:write(IoDevice, ("[PID] ["++Type++"] "++Time++" - "
                        ++Message++"\n")),
            logger_loop(IoDevice);
		{stop} ->
			stop(IoDevice)
	end.
