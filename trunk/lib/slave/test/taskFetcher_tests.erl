%%% @private
-module(taskFetcher_tests).
-include_lib("eunit/include/eunit.hrl").

%% tests the case handle_cast({Pid, error, CallState}, State) ->
%% in taskFetcher
errournous_user_app_value_test_() ->
    {setup,
        fun () ->
                taskFetcher:start_link(),
                application:start(chronicler),
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
                        ?_assertEqual("taskFetcher : Process 1 exited unexpected with state state.\n",
                            io:get_line(File, ""))
                    ]}
        end
    }.
