-module(slave_SUITE).
-compile(export_all).

all() ->
    [unittest].

unittest(_Config) ->
    ok = eunit:test("../../lib/slave/test", []),
    ok.

