-module(chronicler_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() ->
    [
        info_log_test,
        error_log_test,
        debug_log_test,
        warning_log_test,
        user_info_log_test,
        child_specs_test
    ].

% required, but can just return Config. this is a suite level setup function.
init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(_Config) ->
    ok.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(_TestCase, Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    Filename = error_logger:logfile(filename),
    {ok, File} = file:open(Filename, read),
    [{filePointer, File} | Config].

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(_TestCase, [{filePointer, File}]) -> % do custom per suite cleanup here
    ok = file:close(File),
    ok = application:stop(chronicler),
    ok = application:stop(common),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(chronicler),
    ok = application:stop(common),
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

testing_log(File, LoggingLevel, LevelString, Msg, Arg) ->
    chronicler:set_logging_level([LoggingLevel]),
    ok = chronicler:LoggingLevel(Msg, Arg),
    timer:sleep(500),
    %"\n" = io:get_line(File, ""),
    %"=INFO REPORT=" ++ _ = io:get_line(File, ""),
    %"Chronicler application started\n" = io:get_line(File, ""),
    "\n" = io:get_line(File, ""),
    true = lists:prefix(LevelString, io:get_line(File, "")),
    true = lists:prefix(lists:flatten(io_lib:format(Msg, Arg)), io:get_line(File, "")),
    ok.

info_log_test([{filePointer, File}]) ->
    testing_log(File, info, "=INFO REPORT=", "This is a info ~p", [test]);
info_log_test(_Config) ->
    ok.

error_log_test([{filePointer, File}]) ->
    testing_log(File, error, "=ERROR REPORT=", "This is a error ~p", [test]);
error_log_test(_Config) ->
    ok.

debug_log_test([{filePointer, File}]) ->
    testing_log(File, debug, "=INFO REPORT=", "This is a debug ~p", [test]);
debug_log_test(_Config) ->
    ok.

warning_log_test([{filePointer, File}]) ->
    testing_log(File, warning, "=WARNING REPORT=", "This is a warning ~p", [test]);
warning_log_test(_Config) ->
    ok.

user_info_log_test([{filePointer, File}]) ->
    testing_log(File, user_info, "=INFO REPORT=", "This is a user_info ~p", [test]);
user_info_log_test(_Config) ->
    ok.

child_specs_test(_Config) ->
    {ok, {_, ChildSpecs}} = chronicler_sup:init(no_args),
    ok = supervisor:check_childspecs(ChildSpecs).
