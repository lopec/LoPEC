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
    [listener_test,
        input_error,
        pause_resume_test].

init_per_suite(Config) ->
    % do custom per suite setup here
    error_logger:tty(false),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(_Config) ->
    ok.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(pause_resume_test, _Config) ->
    db:start_link(test),
    listener:start_link(),
    {ok, JobId} = listener:add_job(wordcount, mapreduce, kalle, 5, "/storage/test/lol.txt"),
    [{pause_id, JobId}];
init_per_testcase(_TestCase, Config) ->
    Config.

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(pause_resume_test, {pause_id, _JobId}) ->
    db:stop(),
    examiner:stop(),
    listener:stop(),
    dispatcher:stop(),
    timer:sleep(2000),

    %TODO remove job when that functionality has been added.
    %listener:remove_job(JobId).
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

input_error(_Config) ->
    {error, inputdata_dont_exist} = listener:add_job(wordcount, mapreduce,
        kalle, 5,
        "This_File_Should_Never_Exist_Because_The_World_Will_Fall_If_It_Does").

pause_resume_test([{pause_id, JobId}]) ->
    ok = listener:pause_job(JobId),
    ok = listener:resume_job(JobId),
    ok.

listener_test(_Config) ->
    ok.
