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
-module(statistician_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    application:start(chronicler),
    application:start(common),
    statistician:start_link(master).
    
update_test() ->
    {Mega, Sec, Milli} = now(),
    Nu = list_to_integer(
           integer_to_list(Mega) ++
           integer_to_list(Sec) ++
           integer_to_list(Milli)),
    statistician:update({{"lolnode1", Nu, split}, 123, 456, 789, 101, 213}),
    statistician:update({{"lolnode2", Nu, map}, 11, 12, 13, 14, 15}),
    statistician:update({{"lolnode3", Nu, reduce}, 0, 1, 2, 4, 8}),%insert new
    statistician:update({{"lolnode3", Nu, reduce}, 0, 1, 2, 4, 8}),%update it
    statistician:update({{"lolnode4", Nu, finalize}, 30, 40, 70, 10, 20}).
    
get_job_stats_test() ->
    {Mega, Sec, Milli} = now(),
    Nu = list_to_integer(
           integer_to_list(Mega) ++
           integer_to_list(Sec) ++
           integer_to_list(Milli)),
    St1 = statistician:get_job_stats(Nu),
    ?assert(St1 == {error, no_such_job_in_stats}),
    statistician:update({{"lolnode", Nu, split}, 1,6,9,1,3}),
    St2 = statistician:get_job_stats(Nu),
    ?assertNot(St2 == {error, no_such_job_in_stats}).

job_finished_test() ->
    {Mega, Sec, Milli} = now(),
    Nu = list_to_integer(
           integer_to_list(Mega) ++
           integer_to_list(Sec) ++
           integer_to_list(Milli)),
    
    statistician:update({{"lolnode", Nu, split}, 1,6,9,1,3}),
    St1 = statistician:get_job_stats(Nu),
    ?assertNot(St1 == {error, no_such_job_in_stats}),
    statistician:job_finished(Nu),
    timer:sleep(3000),
    St2 = statistician:get_job_stats(Nu),
    ?assert(St2 == {error, no_such_job_in_stats}).

