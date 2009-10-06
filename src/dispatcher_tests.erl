%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Contains the unit tests for the dispatcher. As this requires a
%%% table to be created and contain data we know exatly, these tests
%%% must not be run while the system is live.
%%%
%%% @end
%%% Created : 1 Oct 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher_tests).

-include_lib("eunit/include/eunit.hrl").


get_task_test() ->
    db:start(),
    db:create_tables(),
    TestJob = somestuff,
    db:add_job({TestJob}),
    TestJob = dispatcher:get_work(),
    TestTask = otherstuff,
    db:add_job({TestJob}),
    db:add_task(TestTask),
    TestItem = dispatcher:get_work(),

    case TestItem of
        TestJob ->
            TestItem2 = dispatcher:get_work(),
            case TestItem2 of
                TestTask ->
                    ok;
                Other ->
                    exit("Was expecting TestTask after getting TestJob,"
                         "got ~p~n", [Other])
            end;
        TestTask ->
            exit("Was expecting TestJob; got TestTask.");
        Other ->
            exit("Was expecting TestJob; got ~p~n", [Other])
    end.
