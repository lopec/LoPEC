%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @doc 
%%%
%%% @end
%%% Created : 14 Oct 2009 by Vasilij <>
%%%-------------------------------------------------------------------
-module(booter).
-export([init_master/0, init_slave/0]).

%% Start master node
%% erl -sname server -pa common/ebin -pa ecg/ebin -pa logger/ebin -pa master/ebin -run booter init_master

%% Start slave node
%% erl -pa common/ebin -pa ecg/ebin -pa logger/ebin -pa slave/ebin -run booter init_slave -name 'vj@130.238.15.217' -setcookie supercookie 

init_master() ->
    application:start(chronicler),
    application:start(ecg),
    application:start(common),
    application:start(master),
    db:create_tables(),
    nodes(),
    global:registered_names().

init_slave() ->
    application:start(chronicler),
    application:start(common),
    application:start(slave),
    nodes(),
    global:registered_names().