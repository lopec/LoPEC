%%% @private
-module(fileLogger_tests).
-include_lib("eunit/include/eunit.hrl").

write_test() ->
    {ok, Fd} = fileLogger:init(testFile),
    {ok, Fd} = fileLogger:handle_event(kalle, Fd),
    {ok, Fd} = fileLogger:handle_event(anka, Fd),
    {error, implementationNeeded} = fileLogger:handle_call(request, state),
    {error, implementationNeeded} = fileLogger:handle_info(info, state),
    {error, implementationNeeded} = fileLogger:code_change(oldVsn, state, extra),
    ok = fileLogger:terminate(no_args, Fd),
    {ok, File} = file:open(testFile, read),
    "***Error*** kalle\n" =  io:get_line(File, ""),
    "***Error*** anka\n" = io:get_line(File, ""),
    file:close(File),
    os:cmd("rm testFile").
