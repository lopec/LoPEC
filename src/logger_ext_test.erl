-module(logger_ext_test).

%% This cannot be done simple with EUnit()
% $DateLastEdited$

report_from_client_test() ->
    ServerPid = spawn(logger_ext, start, ["test.txt"]),
    ClientPid = spawn(logger_int, start, [ServerPid, [error]]),
    ClientPid ! {event, self(), "This is a message to Client which will be 
        relayed to server"},
    ClientPid ! {event, self(), error, "This sends a logmessage with type
        ´error´"},
    ClientPid ! {event, self(), info, "This message should be discarded"},
    ClientPid ! {stop},
    ServerPid ! {stop}.
