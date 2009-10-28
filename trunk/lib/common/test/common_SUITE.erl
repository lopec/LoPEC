-module(common_SUITE).
-compile(export_all).

all() ->
    [unittest].

unittest(_Config) ->
    ok = eunit:test("../../lib/common/test", []),
    ok.

