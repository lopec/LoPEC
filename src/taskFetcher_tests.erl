%%% @private
-module(taskFetcher_tests).
-include_lib("eunit/include/eunit.hrl").

-export([get_jobs/0]).

get_jobs() -> [2,4,1,5].

request_job_test() ->
    {ok, Pid} = taskFetcher:start_link(?MODULE),
    2 = taskFetcher:request_job(),
    4 = taskFetcher:request_job(),
    1 = taskFetcher:request_job(),
    5 = taskFetcher:request_job().
