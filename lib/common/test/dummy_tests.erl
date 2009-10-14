%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% This file only exists to ensure that our Makefile works - without
%%% any tests here there would be no .beam files after using make, and
%%% whenever one tried to use make clean it would stop immediately as
%%% there were no test .beam files to clean away.
%%%
%%% @end
%%% Created : 14 Oct 2009 by Axel <>

-module(dummy_tests).
-include_lib("eunit/include/eunit.hrl").

not_a_test() ->
    ok.
