%%% @private
-module(computingProcess_tests).
-include_lib("eunit/include/eunit.hrl").


wrapper_starting_c_port_test_() ->
    ?_assertMatch({ok, _Pid},computingProcess:start_link("tests/port_test",
						      "map","a","asdf")),
    ?_assertMatch(ok,computingProcess:stop()).
