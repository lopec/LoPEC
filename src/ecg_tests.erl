%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: TODO: Add description to ecg_tests
-module(ecg_tests).
-include("global.hrl").

run_test_() ->
    {setup,
     fun () -> application:start(ecg) end,
     fun (_) -> ok end,
     fun (_) -> testing_ecg() end}.

%% TODO: Add description of test_update_list/function_arity
testing_ecg() ->
    Nodes = [compnode1, compnode2, compnode666],

    error_logger:info_msg("Starting: ~p~n"
                          "They should all come up and die.~n",
                          [Nodes]),
    [slave:start_link("localhost", CompNode) || CompNode <- Nodes],

    ecg_server:accept_message({badMsg, [b, s,s]}),
    ecg_server:accept_message({stuff, [w, s,ss]}),

    [slave:stop(CompNode) || CompNode <- Nodes].
