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
 

init_per_test_case() ->    
    db:create_tables().

end_per_test_case() ->
    db:delete_tables().

init_test() ->
    application:start(chronicler),
    application:start(ecg),
    db:start(),
    dispatcher:start_link(),
    listener:start_link().

job_creation_test() ->
    init_per_test_case(),
    %testing if job is created properly
    {ok, Root} = 
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    
    InputFile = Root ++ "input.file",
    JobId = listener:new_job(raytracer, InputFile),
    Job = db:get_job_info(JobId),
    chronicler:info(Job),
    ?assertEqual(JobId, Job#job.job_id),
    ?assertEqual(raytracer, Job#job.job_type),

    
    %Testing if task was properly created
    JobIdString = lists:flatten(io_lib:format("~p", [JobId])),
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