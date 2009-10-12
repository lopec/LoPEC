%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% 
%%% @end
%%% Created : Oct 6, 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------

-module(master_sup_tests).
-include_lib("eunit/include/eunit.hrl").

child_test() ->
    {ok, {_, ChildSpecs}} = master_sup:init(no_args),
    ok = supervisor:check_childspecs(ChildSpecs).