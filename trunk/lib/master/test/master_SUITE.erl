-module(master_SUITE).
-compile(export_all).

all() ->
    [unittest].

init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    Config.

unittest(_Config) ->
    ok = eunit:test("../../lib/master/test", []),
    ok.

