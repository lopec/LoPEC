-module(logger_ext).
-export([start/1]).
-vsn('$Rev: 15 $').

-ifdef(TEST).
-compile(export_all).
-endif.
 
%%%===================================================================
%%% Exported functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Opens a file with appendflag. If a file exist it appends new 
%% information to it, else it just create it. Writes a 'start'-message
%% and then calls logger_loop
%%
%%
%% @spec start(Filename::string())
%% @end
%%--------------------------------------------------------------------
start(Filename) ->
	{ok, IoDevice} = file:open(Filename, [raw,append]),
	file:write(IoDevice, "Starting logger for JobID: "++Filename++"\n"),
	logger_loop(IoDevice).


%%%===================================================================
%%% Internal Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Writes a 'bye'-message to the logfile and closes it.
%%
%% @spec stop(IoDevice) -> ok | {error, Reason} 
%% @end
%%--------------------------------------------------------------------
stop(IoDevice) ->
	file:write(IoDevice, "Stopping logger\n"),
	file:close(IoDevice).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%  Listens to event-messages from other processes. 
%%
%% @spec logger_loop(IoDevice)
%% @end
%%--------------------------------------------------------------------
logger_loop(IoDevice) ->
	receive 
		{event, _PID, Message} -> 
			Time = logger_dh_date:format("H:i:s", erlang:localtime()),
			file:write(IoDevice, ("[PID] "++Time++" - "++Message++"\n")),
			logger_loop(IoDevice);
        {event, _PID, Type, Message} ->
            Time = logger_dh_date:format("H:i:s", erlang:localtime()),
            file:write(IoDevice, ("[PID] ["++atom_to_list(Type)++"] "++Time++
                        " - "++Message++"\n")),
            logger_loop(IoDevice);
		{stop} ->
			stop(IoDevice)
	end.
