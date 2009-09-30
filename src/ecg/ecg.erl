%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% ElectroCardioGram - process that keeps track of alive nodes
%%% @end
%%% Created : 29 Sep 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------
-module(ecg).
-behaviour(gen_server).
-revision('$Rev$').
-created_by('Vasilij Savin').
-author("Vasilij Savin, Gustav Simonsson").

%% Exported Init - do not touch this
-export([init/1, start_link/0]).

%%%===================================================================
%%% Interface Function
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Boots up ECG - cluster heartbeat listener.
%% IMPORTANT: 'logger' should be registered process, otherwise
%% ECG will fail.
%% 
%% @end
%%--------------------------------------------------------------------
init(Args) -> 
    net_kernel:monitor_nodes(true),
    %%for debugging
    %%register (logger, spawn_link(logger_ext, start, ["test.logging"])),
	logger ! {event, self(), "ECG is up and running!"},
    % global:register_name(ecg, self()),
    loop().

%%%===================================================================
%%% Interface Function
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% gen_server callback function.
%% 
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ecg}, ecg, [], []).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Listens to new nodes joining cluster and frees tasks, if node dies.
%% 
%% @end
%%--------------------------------------------------------------------
loop() ->
    receive
        {nodeup, Node} ->
            logger ! {event, self(), 
                io_lib:format("Welcome new node: ~w", [Node])};
        {nodedown, Node} ->
            % Stub needed to contact Task List API
            % tasklist:free_tasks(Node),
			logger ! {event, self(),
                      io_lib:format("Node ~w just died. :()~n", [Node])};
        %% We need to establish connection to new node, if not yet connected
        %% This might be obsolete later, depending on comm protocol
		{new_node, Node} ->
		  	case lists:member(Node, nodes()) of
				false ->
					net_adm:ping(Node);
				true ->
					ok
			end;
        UnrecognisedMessage ->
            logger ! {event, self(), 
                      io_lib:format("UnrecognisedMessage: ~w", [UnrecognisedMessage])}
    end,
    loop().
