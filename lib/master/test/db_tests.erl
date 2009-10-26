-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/db.hrl").

add_job_and_task_test() ->
    db:start(),
    db:delete_tables(),
    db:create_tables(),

    Job1 = {ray_tracer, 15},
    JobId1 = db:add_job(Job1),
     
    Task1 = {JobId1, map, './din_pappa', 20},
    
    % Add one task
    TaskId1 = db:add_task(Task1),
    
    % Set it as reserved to busters@node3
    GetTask1 = db:get_task(busters@node3),

    % Store the id
    GetTaskId1 = GetTask1#task.task_id,
    
    % Check that the id's and state is correct
    ?assertEqual(TaskId1, GetTask1#task.task_id),
    ?assertEqual(reserved, db:get_task_state(GetTaskId1)),
    ?assertEqual(JobId1, GetTask1#task.job_id).

add_tasks_test() ->
    Task2 = {3874, map, './din_pappa', 20},
    Task3 = {598, reduce, './din_pappa', 20},
    Task4 = {1945, finalize, './din_pappa', 20},
    Task5 = {5698, split, './din_pappa', 20},
    Task6 = {598, reduce, './din_pappa', 20},
    Task7 = {1945, finalize, './BLARGHARH', 568},

    % Add some more tasks
    TaskId2 = db:add_task(Task2),
    TaskId3 = db:add_task(Task3),
    TaskId4 = db:add_task(Task4),
    TaskId5 = db:add_task(Task5),
    TaskId6 = db:add_task(Task6),
    
    % Try to add another reduce and a finalize task, which shouldn't be added
    % since we already have one exactly the same
    _TaskId6 = db:add_task(Task6),
    _TaskId7 = db:add_task(Task7),

    % There should be five tasks if we list them
    ?assertEqual(5, length(db:list_tasks())),
    db:delete_tables(),
    db:stop().


%% task_assignment_test() ->
%%     % We set the remaining four tasks as reserved to busters@node3
%%     Task1 = db:get_task(busters@node3),
%%     TaskId1 = Task1#task.task_id,
%%     db:mark_done(TaskId1),
%%     Task2 = db:get_task(busters@node3),
%%     TaskId2 = Task2#task.task_id,
%%     db:mark_done(TaskId2),
%%     Task3 = db:get_task(busters@node3),
%%     TaskId3 = Task3#task.task_id,
%%     db:mark_done(TaskId3),
%%     db:get_task(busters@node3),

%%     % There should be five nodes assigned to busters@node3
%%     ?assertEqual(5, length(db:list_node_tasks(busters@node3))),

%%     % Assign the task to busters@node6
%%     db:assign_task(TaskId1, busters@node6),
%%     ?assertEqual([TaskId1], db:list_node_tasks(busters@node6)),
    
%%     % Remove the reservation
%%     db:remove_reservation(TaskId1),
%%     ?assertEqual([], db:list_node_tasks(busters@node6)).

%% set_functions_test() ->
%%     db:add_task({247, map, 'kldjg', 246}),

%%     % There should be one available task in the table now
%%     Task1 = db:get_task(busters@node666),
%%     TaskId1 = Task1#task.task_id,

%%     % Get the only job in the db
%%     JobId = db:get_job(),

%%     % Test that set_job_input_path works
%%     db:set_job_input_path(JobId, './test'),
%%     Job = db:get_job_info(JobId),
%%     ?assertEqual('./test', Job#job.input_path),

%%     % Test that mark_done works as it should
%%     db:mark_done(TaskId1),
%%     ?assertEqual(done, db:get_task_state(TaskId1)),

%%     % Now we will test the free_tasks(NodeId)-function
%%     ListOfTasks = db:list_node_tasks(busters@node3),
%%     ?assertEqual(4, length(ListOfTasks)),

%%     db:free_tasks(busters@node3),
%%     ?assertEqual([], db:list_node_tasks(busters@node3)),

%%     Test = fun(H) ->
%% 		  ?assertEqual(available, db:get_task_state(H))
%% 	   end,
%%     lists:foreach(Test, ListOfTasks),

%%     db:delete_tables(),
%%     db:stop().


