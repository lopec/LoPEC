%%%-------------------------------------------------------------------
%%% @author Burbas, Vasilij Savin <>
%%% @doc
%%%
%%% Contains the unit tests for the job listener.
%%% rewritten as common_test by sedrik
%%%
%%% @end
%%% Created : 14 Oct 2009 by Vasilij <>
%%%-------------------------------------------------------------------
-module(listener_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").


-include("../include/db.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     listener_test,
     input_error,
     pause_resume_test
    ].

init_per_suite(Config) ->
    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    ok = application:start(master),
    error_logger:tty(false),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(master),
    ok = application:stop(ecg),
    ok = application:stop(common),
    ok = application:stop(chronicler).



init_per_testcase(pause_resume_test, Config) ->
    {ok, JobId} = listener:add_job(wordcount, mapreduce, kalle, 5,
                                   "/storage/test/lol.txt"),
    [{pause_id, JobId} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(pause_resume_test, [{pause_id, JobId} | _Config]) ->
    db:remove_job(JobId),
    [] = db:list(job);
end_per_testcase(_TestCase, _Config) ->
    ok.



%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

input_error(Config) ->
    {error, enoent} =
        listener:add_job(wordcount, mapreduce, kalle, 5,
                         "This_File_Should_Not_Exist"),
    Config.

pause_resume_test([{pause_id, JobId} | Config]) ->
    ok = listener:pause_job(JobId),
    Job = db:get_job(JobId),
    paused = Job#job.state,
    ok = listener:resume_job(JobId),
    PausedJob = db:get_job(JobId),
    free = PausedJob#job.state,
    Config.

listener_test(Config) ->
    Config.
