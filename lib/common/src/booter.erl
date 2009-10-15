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
%% erl -sname client -pa common/ebin -pa ecg/ebin -pa logger/ebin -pa master/ebin/ -pa slave/ebin -run booter init_slave

init_master() ->
    application:start(chronicler),
    application:start(ecg),
    application:start(common),
    application:start(master),
    db:create_tables().
init_slave() ->
    application:start(chronicler),
    application:start(common),
    application:start(slave).
