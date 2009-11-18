%%%-------------------------------------------------------------------
%%% @author Fabian Bergstrom <>
%%% @copyright (C) 2009, Fabian Bergstrom
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2009 by Fabian Bergstrom <>
%%%-------------------------------------------------------------------
-module(janitor_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    {ok, ClusterRoot} =
        configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    JobId = 10101,
    JobRoot = lists:concat([ClusterRoot, "/tmp/", JobId]),
    [filelib:ensure_dir(JobRoot ++ Task)
     || Task <- ["/input/", "/map/", "/reduce/", "/results/"]],
    [{test_job_id, JobId}, {cluster_root, ClusterRoot} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = janitor:start_link(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    exit(whereis(janitor), normal).

all() ->
    [test_cleanup_split, test_cleanup_map, test_cleanup_reduce,
     test_cleanup_finalize, test_cleanup_job].

%% Test cases starts here.
%%--------------------------------------------------------------------

test_cleanup_job() ->
    [{doc, "Test cleanup_job/1"}].

test_cleanup_job(Config) ->
    {test_job_id, JobId} = proplists:lookup(test_job_id, Config),
    {cluster_root, ClusterRoot} = proplists:lookup(cluster_root, Config),
    ok = janitor:cleanup_job(JobId),
    {error, enoent} = file:list_dir(lists:concat([ClusterRoot, "/tmp/", JobId])).

test_cleanup_split() ->
    [{doc, "Test cleanup_split/1"}].

test_cleanup_split(Config) ->
    test_cleanup_fun(fun janitor:cleanup_split/1, "input", Config).

test_cleanup_map() ->
    [{doc, "Test cleanup_map/1"}].

test_cleanup_map(Config) ->
    test_cleanup_fun(fun janitor:cleanup_map/1, "map", Config).

test_cleanup_reduce() ->
    [{doc, "Test cleanup_reduce/1"}].

test_cleanup_reduce(Config) ->
    test_cleanup_fun(fun janitor:cleanup_reduce/1, "reduce", Config).

test_cleanup_finalize() ->
    [{doc, "Test cleanup_finalize/1"}].

test_cleanup_finalize(Config) ->
    test_cleanup_fun(fun janitor:cleanup_finalize/1, "results", Config).

test_cleanup_fun(Fun, DirName, Config) ->
    {test_job_id, JobId} = proplists:lookup(test_job_id, Config),
    {cluster_root, ClusterRoot} = proplists:lookup(cluster_root, Config),
    ok = Fun(JobId),
    {ok, Dirs} = file:list_dir(lists:concat([ClusterRoot, "/tmp/", JobId])),
    false = lists:member(DirName, Dirs).
