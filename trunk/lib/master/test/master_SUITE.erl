-module(master_SUITE).
-compile(export_all).

all() ->
    [unittest].

unittest(_Config) ->
    ok = eunit:test("../../lib/master/test", []),
    ok.

