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

listener_test_() ->
    {setup,
     fun () -> 
        application:start(chronicler),
        chronicler:set_logging_level(all),
        application:start(ecg),
        application:start(common),
        db:start_link(test),
        listener:start_link(),
        dispatcher:start_link(),
        {ok, Root} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
        {ok, JobId} = listener:add_job(raytracer, mapreduce, owner, 0, Root++"lol.txt"),
        {ok, JobId2} = listener:add_job(raytracer, mapreduce, owner2, 1, Root++"lol.txt", "ApanJansson"),
        {JobId, JobId2}
     end,
     fun (_) ->  
         application:stop(chronicler),
         application:stop(ecg),
         application:stop(common),
         db:stop() 
     end,
     fun ({JobId, JobId2}) ->
             {inorder,
              [
                ?_assertEqual(anonymous, listener:get_job_name(JobId)),
                ?_assertEqual({name, "ApanJansson"}, listener:get_job_name(JobId2)),
                ?_assertEqual(ok, listener:pause_job(JobId)),
                ?_assertEqual(anonymous, listener:get_job_name(123)), %% This is a non-existing job
                ?_assertEqual(ok, listener:resume_job(JobId))
              ]
             }
     end
    }.
