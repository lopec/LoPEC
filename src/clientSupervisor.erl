%%% Author  : Sedrik
%%% Description : The client supervisor
%%% Created : Tue Sep 29 08:58:17 CEST 2009

%-vsn('$Rev$').

-module(clientSupervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, printer/1]).

start_link() ->
    supervisor:start_link(?MODULE, no_args).

init(no_args) ->
    {ok,{{one_for_one, 1, 60},
            [{printer, {?MODULE, printer, [no_args]},
                 permanent, brutal_kill, worker, [clientSupervisor]}]
        }}.

printer(no_args) ->
    io:format("Printer: Hello world ~n"),
    timer:sleep(1000),
    printer(no_args).
