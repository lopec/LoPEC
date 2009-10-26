%%% @private
-module(taskFetcher_tests).
-include_lib("eunit/include/eunit.hrl").

%Old test, the code in taskFetcher has been changed but not the test..

%-export([get_jobs/0]).

%get_jobs() -> [2,4,1,5].

%request_job_test_() ->
%    {setup,
%     fun () -> {ok, Pid} = taskFetcher:start_link(?MODULE), Pid end,
%     fun (Pid) -> exit(Pid, kill) end,
%     fun (_Pid) ->
%             {inorder,
%              [?_assertEqual(2, taskFetcher:request_job()),
%               ?_assertEqual(4, taskFetcher:request_job()),
%               ?_assertEqual(1, taskFetcher:request_job()),
%               ?_assertEqual(5, taskFetcher:request_job())]}
%     end
%    }.
