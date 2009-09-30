%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: TODO: Add description to ecg_tests
-module(ecg_tests).
-include("../global.hrl").

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
update_list_test() -> 
	ok.

%% TODO: Add description of test_bury_dead/function_arity
bury_dead_test() ->
	Table = create_ets(),
	ecg:bury_dead(Table),
	test_bury(Table).    

test_bury(Table) ->
    ?_assert(length(ets:match_object(Table, {'_', 0})) =:= 0). 


%%
%% TODO: Add description of test_loop/function_arity
%%
loop_test() ->
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
    Table = ets:new(table),
    ets:insert(Table, [{1, TTL}, {2, TTL}, {3, TTL}, {4, TTL}, {5, 0}]),
	Table.

