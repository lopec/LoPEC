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
    {setup,
     fun () -> master_sup:init(no_args) end,
     fun (_) -> ok end,
     fun ({ok, {_, ChildSpecs}}) ->
             {inorder,
              [
               ?_assertMatch({ok, {_, ChildSpecs}}, master_sup:init(no_args)),
               ?_assertEqual(ok, supervisor:check_childspecs(ChildSpecs))
              ]}
     end
    }.
