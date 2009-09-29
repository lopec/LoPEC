%%% Author  : Sedrik
%%% Description : The client supervisor
%%% Created : Tue Sep 29 08:58:17 CEST 2009

-module(clientSupervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, printer/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

init(no_args) ->
    {ok,{{one_for_one, 1, 60},
            [{printer, {?MODULE, printer, [no_args]},
                 permanent, brutal_kill, worker, [clientSupervisor]},
             {heartbeat, {heartbeat, init, [printer]},
                 permanent, brutal_kill, worker, [heartbeat]}]
        }}.

printer(no_args) ->
    receive
        Anything ->
            io:print("Got message: ~p ~n", [Anything]),
            printer(no_args)
    end.
