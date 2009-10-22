%%%-------------------------------------------------------------------
%%% @author Burbas, Vasilij Savin <>
%%% @doc
%%%
%%% Contains the unit tests for the job listener. 
%%%
%%% @end
%%% Created : 14 Oct 2009 by Vasilij <>
%%%-------------------------------------------------------------------
-module(listener_tests).
-include("../include/db.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The way to run this test only, merge 3 lines below and run in console
%% erl -sname test 
%% -pa ../../common/ebin -pa ../../ecg/ebin -pa ../../logger/ebin -pa ../ebin 
%% -run listener_tests test
%% erl -sname server -pa common/ebin -pa ecg/ebin -pa logger/ebin -pa master/ebin
 
init_test() ->
    application:start(chronicler),
    application:start(ecg),
    application:start(common),
    application:start(master).

init_per_test_case() ->
    db:delete_tables(),  
    db:create_tables().

end_per_test_case() ->
    db:delete_tables().

job_name_test_() ->
    {setup,
     fun () -> init_per_test_case(),
               {ok, Root} = 
                   configparser:read_config("/etc/clusterbusters.conf",
                                            cluster_root),
               {listener:new_job(raytracer, Root ++ "ray256", ray256),
                listener:new_job(raytracer, Root ++ "ray256", no_name)
               }
     end,
     fun (_) -> end_per_test_case() end,
     fun ({Named, Anonymous}) ->
             {inorder,
              [?_assertEqual({name, ray256},
                             listener:get_job_name(Named)),
               ?_assertEqual(anonymous,
                             listener:get_job_name(Anonymous)),
               ?_assertEqual(ok,
                             listener:remove_job_name(Named)),
               ?_assertEqual(anonymous,
                             listener:get_job_name(Named))                             
              ]
             }
     end
    }.

job_creation_test() ->
    init_per_test_case(),
    %testing if job is created properly
    {ok, Root} = 
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    
    InputFile = Root ++ "ray256",
    JobId = listener:new_job(raytracer, InputFile),
    Job = db:get_job_info(JobId),
    chronicler:info(Job),
    ?assertEqual(JobId, Job#job.job_id),
    ?assertEqual(raytracer, Job#job.job_type),
    
    %Testing if task was properly created
    %JobIdString = lists:flatten(io_lib:format("~p", [JobId])),
    JobIdString = integer_to_list(JobId),
    TaskList = db:list_tasks(JobId),
    ?assertMatch([_A], TaskList),
    Task = db:get_task_info(hd(TaskList)),
    ?assertEqual(split, Task#task.task_type),
    ?assertEqual(JobId, Task#task.job_id),
    % Testing if input file was created
    % If assertion fails, check the path.
    % input.file should exist in storage before test
    ProgramFile = Root ++ "tmp/" ++ JobIdString ++ "/input/data.dat",
    chronicler:info(ProgramFile),
    ?assertEqual(ok, file:rename(ProgramFile, ProgramFile)),
    end_per_test_case().

end_test() ->
    db:stop(),
    dispatcher:terminate(finished, []).
