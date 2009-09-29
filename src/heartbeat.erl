%%% File    : heartbeat.erl
%%% Author  : Axel <>
%%% Description : Implementation of node heartbeat process
%%% Created : 28 Sep 2009 by Axel <>

-module(heartbeat).
-export([init/1, start/1]).

-revision('Revision: 1.0b').
-created('Date: Monday, September 28 2009').
-created_by('axelandren@gmail.com').
-modified('Tuesday, September 29 2009').
-modified_by('axelandren@gmail.com').
-description('Heartbeat process sends a message to electrocardiogram
process every so often (see definition of CARDIAC_FREQUENCY below) to
signal that the node the heartbeat process belongs to is still alive.').


-define(CARDIAC_FREQUENCY, 1000). % 1000 = 1 second

start(ECG_PID) ->
    spawn(?MODULE, init, [ECG_PID]). % Fabian suggests this is necessary

% TODO: find out if we need to manually terminate the heartbeat process

init(ECG_PID) ->
    loop(ECG_PID).


loop(ECG_PID) ->
    timer:sleep(?CARDIAC_FREQUENCY),
    ECG_PID ! {heartbeat, self()},
    % TODO: find out if ECGs need more items in the heartbeat messages
    loop(ECG_PID).
		
