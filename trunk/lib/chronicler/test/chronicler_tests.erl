%% Author: Sedrik
%%% @private
-module(chronicler_tests).
-include_lib("eunit/include/eunit.hrl").

testing_log(LoggingLevel, LevelString, Msg, Arg) ->
    {setup,
        fun start_logger/0,
        fun stop_logger/1,
        fun (File) ->
                {inorder,
                    [
                        ?_assertEqual(ok, chronicler:LoggingLevel(Msg, Arg)),
                        %?_assertMatch("\n", io:get_line(File, "")),
                        %?_assertMatch("=INFO REPORT=" ++ _, io:get_line(File, "")),
                        %?_assertMatch("Chronicler application started\n",
                        %    io:get_line(File, "")),
                        ?_assertMatch("\n", io:get_line(File, "")),
                        ?_assert(lists:prefix(LevelString, io:get_line(File, ""))),
                        ?_assert(lists:prefix(lists:flatten(io_lib:format(Msg, Arg)),
                                io:get_line(File, "")))
                    ]}
        end
    }.

info_log_test_() ->
    testing_log(info, "=INFO REPORT=", "This is a info ~p", [test]).

error_log_test_() ->
    testing_log(error, "=ERROR REPORT=", "This is a error ~p", [test]).

debug_log_test_() ->
    testing_log(debug, "=INFO REPORT=", "This is a debug ~p", [test]).

warning_log_test_() ->
    testing_log(warning, "=WARNING REPORT=", "This is a warning ~p", [test]).

user_info_log_test_() ->
    testing_log(user_info, "=INFO REPORT=", "This is a user_info ~p", [test]).


%set_logging_level_test() ->
%    application:start(common),
%    application:start(chronicler),
%    chronicler:set_logging_level([info]),
%    timer:sleep(3000),
%    chronicler:info("logging level set to all"),
%    application:stop(chronicler),
%    application:stop(common).

start_logger() ->
    application:start(common),
    application:start(chronicler),
    {ok, File} = file:open(chronicler:fetch_logfile(), read),
    chronicler:set_logging_level([all]),
    File.

stop_logger(File) ->
    application:stop(chronicler),
    application:stop(common),
    error_logger:logfile(close),
    file:close(File).
