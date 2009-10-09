%%%-------------------------------------------------------------------
%%% @author Axel <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Contains the unit tests for the dispatcher. As this requires a
%%% table to be created and contain data we know exactly, these tests
%%% must not be run while the system is live.
%%%
%%% @end
%%% Created : 1 Oct 2009 by Axel <>
%%%-------------------------------------------------------------------
-module(dispatcher_tests).
-include("../include/db.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_test_case() ->    
    db:create_tables().

end_per_test_case() ->
    db:delete_tables().

init_test() ->
    db:start(),
    dispatcher:start_link().

task_allocation_test() ->
    init_per_test_case(),
    FreeTask1 = {1, map, "Callback",  "Input3", 0},
    FId = db:add_task(FreeTask1),
    dispatcher:get_task(node(), self()),
    receive
        {requestACK, Task, FinderPID} ->
            io:format("Task received: ~w", [Task]),
            ?assert(Task#task.task_id =:= FId),
            ?assert(db:get_task_state(FId) =:= reserved),
            FinderPID ! {task_accepted, Task#task.task_id, node(), self()},
            ?assert(db:get_task_state(FId) =:= assigned);
        Msg ->
                io:format("Wrong message received: ~w", [Msg])            
    end,
    end_per_test_case().

timeout_test() ->
    init_per_test_case(),
    dispatcher:get_task(node(), self()),
    receive
        Msg ->
            io:format("Unexpected message received: ~w", [Msg])
        after 1000 ->
            ok
    end,
    end_per_test_case().

task_completed_test() ->
    init_per_test_case(),
    
    AssignedTask = {1, map, "Callback",  "Input6", 0},
    AId = db:add_task(AssignedTask),
    db:assign_task(AId, node()),
    ?assertEqual(assigned, db:get_task_state(AId)),
    dispatcher:report_task_done(AId),
    logger ! {event, self(), 
              io_lib:format("Before ~p is ~p!", [AId, db:get_task_state(AId)])},
    Status = db:get_task_state(AId),
    logger ! {event, self(), 
              io_lib:format("After ~p is ~p!", [AId, db:get_task_state(AId)])},
    ?assertEqual(done, Status),
    

    end_per_test_case().
    
end_test() ->
    db:stop().