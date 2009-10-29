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
 
 job_name_test_() ->
     {setup,
        fun () -> 
            application:start(chronicler),
            chronicler:set_logging_level(all),
            application:start(ecg),
            application:start(common),
            listener:start_link(),
            dispatcher:start_link()
        end,
        fun (JobId) ->
            db:remove_job(JobId)
        end,
        fun () ->
            {ok, Root} = configparser:read_config("/etc/clusterbusters.conf", root),
            listener:add_job(raytracer, split, apanjansson, 0, Root)
        end
     }.


end_per_test_case(JobId) ->
    db:remove_job(JobId).

%% job_name_test_() ->
%%     {setup,
%%      fun () -> {ok, Root} = configparser:read_config("/etc/clusterbusters.conf",
%%                                             cluster_root),
%%                [Root,
%%                 listener:add_job
%%                     (raytracer, mapreduce, owner, 0, Root ++ "ray256", ray256),
%%                 listener:add_job
%%                     (raytracer, mapreduce, owner, 0, Root ++ "ray256")
%%                ]
%%      end,
%%      fun (_) -> end_per_test_case() end,
%%      fun ([Root, {ok, Named}, {ok, Anonymous}]) ->
%%              {inorder,
%%               [?_assertEqual(anonymous, listener:get_job_name(Anonymous)),
%%                ?_assertEqual({name, ray256}, listener:get_job_name(Named)),
%%                ?_assertMatch({error, _},
%%                     listener:new_job
%%                    (raytracer, mapreduce, owner, 0, Root ++ "ray256", ray256)),
%%                ?_assertEqual(ok, listener:remove_job_name(Named)),
%%                ?_assertEqual(anonymous, listener:get_job_name(Named))                             
%%               ]
%%              }
%%      end
%%     }.

job_creation_test() ->
    chronicler:debug("Local processes: ~p", [erlang:registered()]),
    chronicler:debug("Global processes: ~p", [global:registered_names()]),
    %testing if job is created properly
    
    {ok, Root} = 
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    
    InputFile = Root ++ "ray256",
    {ok, JobId} = listener:add_job(raytracer, mapreduce, owner, 0, InputFile),
    Job = db:get_job(JobId),
    chronicler:info(Job),
    ?assertEqual(JobId, Job#job.job_id),
    ?assertEqual(raytracer, Job#job.program_name),
    
    %Testing if task was properly created
    JobIdString = integer_to_list(JobId),
    TaskList = db:list(split_free),
    ?assertMatch([_A], TaskList),
    Task = db:get_task(hd(TaskList)),
    ?assertEqual(split, Task#task.type),
    ?assertEqual(JobId, Task#task.job_id),
    % Testing if input file was created
    % If assertion fails, check the path.
    % input.file should exist in storage before test
    ProgramFile = Root ++ "tmp/" ++ JobIdString ++ "/input/data.dat",
    chronicler:info(ProgramFile),
    %%TODO fix default test input file.
    ?assertEqual(ok, file:rename(ProgramFile, ProgramFile)),
    end_per_test_case(JobId).

stop_test()->
    db:stop().

