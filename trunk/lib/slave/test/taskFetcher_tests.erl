%%% @private
-module(taskFetcher_tests).
-include_lib("eunit/include/eunit.hrl").

%% tests the case handle_cast({Pid, error, CallState}, State) ->
%% in taskFetcher
errournous_user_app_value_test_() ->
    {setup,
        fun () ->
                application:start(chronicler),
                taskFetcher:start_link(),
                {ok, File} = file:open(nonode@nohost, read),
                File
        end,
        fun (File) ->
                application:stop(chronicler),
                error_logger:logfile(close),
                file:close(File)
        end,
        fun (File) ->
                {inorder,
                    [
                        ?_assertEqual(ok, gen_server:cast(taskFetcher, {1, error, state})),
                        ?_assertMatch("\n", io:get_line(File, "")),
                        ?_assertMatch("=INFO REPORT=" ++ _, io:get_line(File, "")),
                        ?_assertMatch("Chronicler application started\n", io:get_line(File, "")),
                        ?_assertMatch("\n", io:get_line(File, "")),
                        ?_assertMatch("=INFO REPORT=" ++ _, io:get_line(File, "")),
                        ?_assertMatch("taskFetcher : module started\n", io:get_line(File, "")),
                        ?_assertMatch("\n", io:get_line(File, "")),
                        ?_assertMatch("=WARNING REPORT=" ++ _, io:get_line(File, "")),
                        ?_assertMatch("taskFetcher : Process 1 exited unexpected with state state.\n", io:get_line(File, "")), 
                        ?_assertEqual(ok, gen_server:cast(taskFetcher, "testtest")),
                        ?_assertMatch("\n", io:get_line(File, "")),
                        ?_assertMatch("=WARNING REPORT=" ++ _, io:get_line(File, "")),
                        ?_assertMatch("taskFetcher : Received unexpected handle_cast call.\n", io:get_line(File, ""))

                    ]}
        end
    }.
