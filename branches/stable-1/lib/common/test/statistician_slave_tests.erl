%%%-------------------------------------------------------------------
%%% @author Axel Andren <axelandren@gmail.com>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Tests for the statistician module. big_test was originally just
%%% a script for copypasting into erl console.
%%%
%%% @end
%%% Created : 23 Oct 2009 by Axel <axelandren@gmail.com>
%%%-------------------------------------------------------------------
-module(statistician_slave_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    application:start(chronicler),
    application:start(common),
    %% start_link in another process so we don't die when we kill
    %% statistician in end_test().
    spawn(fun () -> statistician:start_link(slave) end).

slave_test() ->
    {Mega, Sec, Milli} = now(),
    Nu = list_to_integer(
           integer_to_list(Mega) ++
           integer_to_list(Sec) ++
           integer_to_list(Milli)),
    statistician:update({{"lolnode1", Nu, split}, 123, 456, 789, 101, 213}),
    St1 = statistician:get_job_stats(Nu),
    ?assertNot(St1 == {error, no_such_job_in_stats}), 
    timer:sleep(3000), %Should have flushed by now.
    St2 = statistician:get_job_stats(Nu),
    ?assert(St2 == {error, no_such_job_in_stats}). 

end_test() ->
    application:stop(chronicler),
    application:stop(common),
    exit(whereis(statistician), kill).
