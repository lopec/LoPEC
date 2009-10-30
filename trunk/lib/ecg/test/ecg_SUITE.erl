-module(ecg_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [unittest].

init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(Config) ->
    ok.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(unittest, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    ok.

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(unittest, Config) ->
    Config;
end_per_testcase(TestCase, Config) ->
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

unittest(_Config) ->
    ok = eunit:test("../../lib/ecg/test", []),
    ok.

