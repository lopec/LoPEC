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
    % do custom per suite setup here
    ok = application:start(common),
    ok = application:start(chronicler),
    ok = application:start(ecg),
    ok = application:start(master),
    error_logger:tty(false),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(_Config) ->
    ok = application:stop(ecg),
    ok = application:stop(common),
    ok = application:stop(chronicler),
    ok = application:stop(master).

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(pause_resume_test, Config) ->
    {ok, JobId} = listener:add_job(wordcount, mapreduce, kalle, 5, "/storage/test/lol.txt"),
    [{pause_id, JobId} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

input_error(Config) ->
    {error, inputdata_dont_exist} =
        listener:add_job(wordcount, mapreduce, kalle, 5,
        "This_File_Should_Never_Exist_Because_The_World_Will_Fall_If_It_Does"),
    Config.

pause_resume_test([{pause_id, JobId} | Config]) ->
    ok = listener:pause_job(JobId),
    ok = listener:resume_job(JobId),
    Config.

listener_test(Config) ->
    Config.
