%%%-------------------------------------------------------------------
%%% @author Axel, Vasilij Savin <>
%%% @copyright (C) 2009, Axel
%%% @doc
%%%
%%% Contains the unit tests for the dispatcher. 
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
    TaskSpec = {255, split, "", 0},
    io:format("Task created: ~p", [TaskSpec]),
%%     FId =  dispatcher:create_task(TaskSpec),
%%     io:format("Task received: ~p", [FId]),
%%     dispatcher:get_task(node(), self()),
%%     receive
%%         {task_response, Task} ->
%%             io:format("Task received: ~p", [Task]),
%%             ?assert(Task#task_tmp.task_id =:= FId),
%%             ?assertEqual(split, Task#task_tmp.job_type),
%%             ?assertEqual(255, Task#task_tmp.job_id);
%%         Msg ->
%%             io:format("Wrong message received: ~p", [Msg])            
%%     end,
    end_per_test_case().

%% This test case is used to verify that request times out
%% when there is no free tasks, so taskFetcher can try again at later time.
%% timeout_test() ->
%%     init_per_test_case(),
%%     dispatcher:get_task(node(), self()),
%%     receive
%%         Msg ->
%%             io:format("Unexpected message received: ~p", [Msg])
%%         after 1000 ->
%%             ok
%%     end,
%%     end_per_test_case().
%% 
%% task_completed_test() ->
%%     init_per_test_case(),
%%     AssignedTask = {new_task, 1, map, 0},
%%     AId = db:add_task(AssignedTask),
%%     db:assign_task(AId, node()),
%%     ?assertEqual(assigned, db:get_task_state(AId)),
%%     dispatcher:report_task_done(AId),
%%     Status = db:get_task_state(AId),
%%     ?assertEqual(done, Status),
%%     end_per_test_case().
%%     
%% create_job_test() ->
%%     init_per_test_case(),
%%     JobId = db:add_job({raytracer, 0}),
%%     io:format("JobId: ~p", [JobId]),
%%     end_per_test_case().
%% 
end_test() ->
    db:stop(),
    dispatcher:terminate(finished, []).
