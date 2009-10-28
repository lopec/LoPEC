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
        user_info_log_test
    ].

% required, but can just return Config. this is a suite level setup function.
init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(Config) ->
    % do custom per suite cleanup here
    Config.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(TestCase, Config) ->
    application:start(chronicler),
    {ok, File} = file:open(error_logger:logfile(filename), read),
    [{filePointer, File}].

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(TestCase, [{filePointer, File}]) ->
    application:stop(chronicler),
    file:close(File);
end_per_testcase(TestCase, Config) ->
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

testing_log(File, LoggingLevel, LevelString, Msg, Arg) ->
    ok = chronicler:LoggingLevel(Msg, Arg),
    "\n" = io:get_line(File, ""),
    "=INFO REPORT=" ++ _ = io:get_line(File, ""),
    "Chronicler application started\n" = io:get_line(File, ""),
    "\n" = io:get_line(File, ""),
    lists:prefix(LevelString, io:get_line(File, "")),
    lists:prefix(lists:flatten(io_lib:format(Msg, Arg)), io:get_line(File, "")).

info_log_test([{filePointer, File}]) ->
    testing_log(File, info, "=INFO REPORT=", "This is a info ~p", [test]).

error_log_test([{filePointer, File}]) ->
    testing_log(File, error, "=ERROR REPORT=", "This is a error ~p", [test]).

debug_log_test([{filePointer, File}]) ->
    testing_log(File, debug, "=INFO REPORT=", "This is a debug ~p", [test]).

warning_log_test([{filePointer, File}]) ->
    testing_log(File, warning, "=WARNING REPORT=", "This is a warning ~p", [test]).

user_info_log_test([{filePointer, File}]) ->
    testing_log(File, user_info, "=INFO REPORT=", "This is a user_info ~p", [test]).
