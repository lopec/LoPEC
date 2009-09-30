-module(clientApp).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    clientSupervisor:start_link().

stop(_State) ->
    ok.
