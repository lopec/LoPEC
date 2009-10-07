%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: TODO: Add description to ecg_tests
-module(ecg_tests).
-include("global.hrl").
-export([client_listener/0, terminate/0]).

run_test() ->
    init(),
    testing_ecg().
    
    %terminate().

%% Some init
init() ->
    register (logger, spawn_link(logger_ext, start, ["test.logging"])),
    application:start(ecg).

%% TODO: Add description of test_update_list/function_arity
testing_ecg() ->
    io:format("Sending ~n", []),
%%     CompNode1 = list_to_atom("compnode1@" ++ os:cmd("uname -n") -- [$\n]),
%%     CompNode2 = list_to_atom("compnode2@" ++ os:cmd("uname -n") -- [$\n]),
%%     CompNode3 = list_to_atom("compnode666@" ++ os:cmd("uname -n") -- [$\n]),
    
    CompNode1 = list_to_atom("compnode1"),
    CompNode2 = list_to_atom("compnode2"),
    CompNode3 = list_to_atom("compnode666"),
    PID1 = slave:start_link(os:cmd("uname -n"), CompNode1, ""),
    PID2 = slave:start_link(os:cmd("uname -n"), CompNode2, ""),
    PID3 = slave:start_link(os:cmd("uname -n"), CompNode3, ""),
    
    io:format("Calling directly ~n", []),    
    ecg_server:accept_message({new_node, CompNode1}),
    ecg_server:accept_message({new_node, CompNode2}),
    ecg_server:accept_message({new_node, CompNode3}),
    ecg_server:accept_message({badMsg, [b, s,s]}),
    ecg_server:accept_message({stuff, [w, s,ss]}),
    
    slave:stop(PID1),
    slave:stop(PID2),
    slave:stop(PID3),
    io:format("Done ~n", []).

terminate() ->
    exit(whereis(logger), kill),
    application:stop(ecg).
    
client_listener() ->
    receive
        {terminate} ->
            halt();
        _ ->
            io:format("Got wrong message ~n", [])
    end.
