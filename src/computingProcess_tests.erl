%%% @private
-module(computingProcess_tests).
-include_lib("eunit/include/eunit.hrl").

write_test() ->
    {ok, Pid} = computingProcess:start_link("priv/","port"),
    2 = computingProcess:foo(1),
    4 = computingProcess:bar(2),
    ok = computingProcess:stop().
