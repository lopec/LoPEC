%%% File    : heartbeat.erl
%%% Author  : Axel <>
%%% Description : For the client node heartbeat.
%%% Created : 28 Sep 2009 by Axel <>

-module(heartbeat).
-export([init/1, loop/1]).

-define(CARDIAC_FREQUENCY, 1000). % 1000 = 1 second

init(ECGid) ->
    loop(ECGid).


loop(ECGid) ->
    receive
	Anything ->
	    io:print("Error: Heartbeat process ~p got a message: ~n"
		     "~p", [Anything, self()])
    after ?CARDIAC_FREQUENCY ->
	    ECGid ! {heartbeat, self()}
    end.

		
		
