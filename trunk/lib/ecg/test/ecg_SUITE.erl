-module(ecg_SUITE).
-compile(export_all).

all() ->
    [unittest].

unittest(_Config) ->
    ok = eunit:test("../../lib/ecg/test", []),
    ok.

