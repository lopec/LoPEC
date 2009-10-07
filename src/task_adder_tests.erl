%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%% Contains the unit tests for the task adder
%%% @end
%%% Created : 1 Oct 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(task_adder_tests).
-include_lib("eunit/include/eunit.hrl").

receive_details_test() ->
    Stuff = {'./1.job', './1.inp', 6643, 10},
    task_adder:receive_details(Stuff),
    receive
        {reply, _Data, waiting_for_task_details} ->
            ok;
        Other ->
            exit("Failed to receive&add task Stuff;~n"
                 "Got ~p~n", [Other])
    end.
    
