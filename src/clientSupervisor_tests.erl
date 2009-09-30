%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The client supervisor eunit tests
%%% Tests the supervisor
%%% @end
%%% Created : 30 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(clientSupervisor_test).
-include_lib("eunit/include/eunit.hrl").

child_test() ->
    {ok, {_, ChildSpecs}} = clientSupervisor:init(no_args),
    supervisor:check_childspecs(ChildSpecs).

