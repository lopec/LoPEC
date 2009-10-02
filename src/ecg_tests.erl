%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: TODO: Add description to ecg_tests
-module(ecg_tests).
-include("global.hrl").

-export([client_listener/0]).
%%
%% Include files
%%

%%
%% Exported Functions
%%

%% bury_dead_test_/0, 
%%
%% API Functions
%%

%% TODO: Add description of test_update_list/function_arity
ecg_test() ->
    register (logger, spawn_link(logger_ext, start, ["test.logging"])),
    ecg_server:start_link(),
    io:format("Sending ~n", []),
	ecg_server:accept_message({new_node, 'compnode1@Workstation'}),
    ecg_server:accept_message({badMsg, [a, b,c]}),
    ecg_server:accept_message({stuff, [a, b,c]}),
    io:format("Done ~n", []).
    %exit(whereis(ecg), kill).
    %exit(whereis(logger), kill).

client_listener() ->
    receive
        {terminate} ->
            halt();
            %os:cmd("kill -9 " ++ os:getpid());
        _ ->
            io:format("Got wrong message ~n", [])
    end.
