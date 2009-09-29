%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: TODO: Add description to ecg_tests
-module(ecg_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([update_list_test_/0, bury_dead_test_/0, loop_test_/0]).

%%
%% API Functions
%%

%%
%% TODO: Add description of test_update_list/function_arity
%%
update_list_test_() -> 
	ok.
%%
%% TODO: Add description of test_bury_dead/function_arity
%%
bury_dead_test_() ->
    {   setup,
        fun create_ets/0,
        fun ecg:bury_dead/1
    }.

%%
%% TODO: Add description of test_loop/function_arity
%%
loop_test_() ->
    ok.
%% 	{setup, 
%%      fun create_ets/0,
%%      fun ecg:init/1
%%     }.


%%
%% Local Functions
%%

create_ets() ->
    TTL = 10,
    Table = ets:new(table, [named_table, private, set]),
    ets:insert(Table, [{1, TTL}, {2, TTL}, {3, TTL}, {4, TTL}, {5, 0}]).

