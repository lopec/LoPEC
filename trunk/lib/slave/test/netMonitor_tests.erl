%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Tests for the statistician module. Was originally just
%%% a script for copypasting into erl console.
%%%
%%% @end
%%% Created : 23 Oct 2009 by Axel <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(netMonitor_tests).
-include_lib("eunit/include/eunit.hrl").

netmonitor_test_() ->
    {inorder,
     [
      ?assert(compare_returns())
     ]}.

compare_returns() ->
    {A, B} = netMonitor:get_net_stats(),
    timer:sleep(1500),
    {C, D} = netMonitor:get_net_stats(),
    (C >= A) and (D >= B).
