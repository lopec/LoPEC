%%% @private
-module(clientSupervisor_tests).
-include_lib("eunit/include/eunit.hrl").

child_test() ->
    {ok, {_, ChildSpecs}} = clientSupervisor:init(no_args),
    ok = supervisor:check_childspecs(ChildSpecs).

