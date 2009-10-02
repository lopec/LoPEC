%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Contains the unit tests for the dispatcher.
%%% @end
%%% Created : 1 Oct 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher_tests).

-include_lib("eunit/include/eunit.hrl").


get_task_test() ->
    db:init(), %TODO: the DB module is not yet made, cannot run tests
    TestJob = somestuff,
    db:add_job({TestJob}),
    TestJob = dispatcher:get_task(),
    TestTask = otherstuff,
    db:add_job({TestJob}),
    db:add_task(TestTask),
    TestItem = dispatcher:get_task(),

    %These "case"s check that we only get the two just added items,
    %and only once each.
    case TestItem of
        TestJob ->
            case TestItem of
                TestTask ->
                    ok;
                Other ->
                    exit("Was expecting TestTask after getting TestJob,"
                         "got ~p~n", [Other])
            end;
        TestTask ->
            case TestItem of
                TestJob ->
                    ok;
                Other ->
                    exit("Was expecting TestJob after getting TestTask,"
                         "got ~p~n", [Other])
            end
    end.
