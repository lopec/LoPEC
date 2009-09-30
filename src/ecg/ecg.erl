%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: ElectroCardioGram - process that keeps track of alive nodes
-module(ecg).
-revision('$Rev$').
-created_by('Vasilij Savin').

%%
%% Exported Init - do not touch this
%%
-export([init/0]).

%% TODO: Add description of init/function_arity
init() -> 
    net_kernel:monitor_nodes(true),
	%%HACK just for testing if ecg works, will be removed later
	LogPID = spawn_link(logger_ext, start, ["test.logging"]),
	register (logger, LogPID),
	logger ! {event, self(), "Start Logging!"},
    register (ecg, self()),
    loop().

%%
%% Local Functions
%%

%%
%% Listens to heartbeats from nodes and removes dead nodes
%%
loop() ->
    receive
        {nodeup, Node} ->
            io:format("Welcome new node: ~w~n", [Node]),
            logger ! {event, self(), 
                io_lib:format("Welcome new node: ~w", [Node])};
        {nodedown, Node} ->
            io:format("Node ~w just died. :(", [Node]),
			logger ! {event, self(),
                      io_lib:format("Node ~w just died. :()~n", [Node])};
			% Stub needed to contact Task List API
			% tasklist:free_tasks(Node),
		{new_node, Node} ->
		  	case lists:member(Node, nodes()) of
				false ->
					%logger ! {event, self(), "New Node comes!"},
					net_adm:ping(Node);
				true ->
					%logger ! {event, self(), "Hey old Dude!"},
					ok
			end;
        UnrecognisedMessage ->
            io:format("~w", [UnrecognisedMessage]),
            logger ! {event, self(), io_lib:format("~w", [UnrecognisedMessage])}
    end,
    loop().