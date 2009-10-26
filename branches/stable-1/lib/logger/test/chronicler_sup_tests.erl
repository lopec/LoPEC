%%% @private
-module(chronicler_sup_tests).
-include_lib("eunit/include/eunit.hrl").

child_init_test_() ->
    ?_assertMatch({ok, {_, _}}, chronicler_sup:init(no_args)).

child_specs_test_() ->
    {ok, {_, ChildSpecs}} = chronicler_sup:init(no_args),
    ?_assertMatch(ok, supervisor:check_childspecs(ChildSpecs)).
