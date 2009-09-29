-module(logger_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
             [{logger_int, {logger_int, start_link, []}, 
                 permanent, brutal_kill, worker, [logger_int]}]}}.
