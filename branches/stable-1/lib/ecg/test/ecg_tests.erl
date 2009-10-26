%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: Testing module, that verifies if ECG is properly monitoring
%% network
-module(ecg_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    ok,
    {setup,
     fun () ->  application:start(chronicler),
                application:start(ecg)
                end,
     %% A dummy functions to satisfy the number of terms in test.
     fun (_) -> ok end,
     fun (_) -> testing_ecg() end}.

testing_ecg() ->
    Nodes = [compnode1, compnode2, compnode666],
    chronicler:info(io_lib:format
       ("Starting: ~p~n They should all come up and die.~n", [Nodes])),
    %% For each element in Nodes, start a slave with the name
    %% equivalent to that element.
    [slave:start_link("localhost", CompNode) || CompNode <- Nodes],
    ecg_server:accept_message({badMsg, [b, s,s]}),
    ecg_server:accept_message({stuff, [w, s,ss]}),

    %% For each element in Nodes, stop the Node associated with
    %% that element.
    [slave:stop(CompNode) || CompNode <- Nodes].