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
    {ok, _TaskAdderPID} = task_adder:start_link(),
    Stuff = task_adder:receive_details(some_task_details),
    case Stuff of
        error -> exit("Failed to add a task.~n");
        %TODO: dont know what  exactly db:add_task, which 
        %receive_details ultimately calls, returns in case of an
        %error.
        _Other -> ok
    %TODO: am assuming that db:add_task will always return errors in
    %some specific way, even if that way is not yet exactly known
    end.
                      
    
