%% Author: chabbrik
%% Created: Sep 29, 2009
%% Description: ElectroCardioGram - process that keeps track of alive nodes
-module(ecg).
-revision('$Rev$').
-created_by('Vasilij Savin').

%%
%% Include files
%%

%%
%% Exported Functions
%%
%%-export([loop/1]).

%%
%% Exported Init - do not touch this
%%
-export([init/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of init/function_arity
%%
init(TTL) -> 
    EcgTable = ets:new(ecg_Table, [named_table, private, set]),
    loop(EcgTable, TTL).

%%
%% Local Functions
%%

%%
%% Listens to heartbeats from nodes and removes dead nodes
%%
loop(EcgTable, TTL) ->
    receive
        {heartbeat, PID} ->
            ets:insert(EcgTable, {PID, TTL});
        {tick} ->
            update_list(EcgTable, ets:first(EcgTable)),
            bury_dead(EcgTable)
    end,
    loop(EcgTable, TTL).

%% Reducing value of each element by one in ETS
update_list(_, '$end_of_table') ->
    ok;
update_list(EcgTable, Key) ->
    {_, NewTTL} = ets:lookup(EcgTable, Key),
    ets:insert(EcgTable, {Key, NewTTL - 1}),
    NextKey = ets:next(EcgTable, Key),
    update_list(EcgTable, NextKey).

%% Remove deadnodes
bury_dead(EcgTable) ->
    ets:match_delete(EcgTable, {'_', 0}).
