%%% Author  : Sedrik
%%% Description : The client supervisor
%%% Created : Tue Sep 29 08:58:17 CEST 2009

-module(clientSupervisor).
-behaviour(supervisor).

-define(HEARTBEAT, heartbeat).

-export([start_link/0]).
-export([init/1, printer/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

init(no_args) ->
    {ok,{{one_for_one, 1, 60},
            [{printer, {?MODULE, printer, [no_args]},
                 permanent, brutal_kill, worker, [clientSupervisor]},
             {?HEARTBEAT, {?HEARTBEAT, init, [whereis(printer)]},
                 permanent, brutal_kill, worker, [?HEARTBEAT]}]
        }}.

printer(no_args) ->
    io:format("Printer: Hello world"),
    receive
        Anything ->
            io:print("Got message: ~p ~n", [Anything]),
            printer(no_args)
    end.
