%%% Author  : Sedrik
%%% Description : The client supervisor
%%% Created : Tue Sep 29 08:58:17 CEST 2009

%-vsn('$Rev$').

-module(logger_ext_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, no_args).

init(WhereToLog) ->
    {ok,{{one_for_one, 1, 60},
            [child(logger_ext, WhereToLog)]}}.

child(Module, Args) ->
    {Module, {Module, start_link, [Args]}, permanent, brutal_kill, worker, [Module]}.
