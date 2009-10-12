%%% @private
-module(clientSupervisor_tests).
-include_lib("eunit/include/eunit.hrl").

child_init_test_() ->
    ?_assertMatch({ok, {_, _}}, clientSupervisor:init(no_args)).

child_specs_test_() ->
    {ok, {_, ChildSpecs}} = clientSupervisor:init(no_args),
    ?_assertMatch(ok, supervisor:check_childspecs(ChildSpecs)).
