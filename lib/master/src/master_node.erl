%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <vasilij.savin@gmail.com>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% This module handles start of master node application.
%%% Currently it is not using any parameters.
%%% TODO: Once logger is up and running, stop function will log when 
%%% application is stopped.
%%% @end
%%% Created : Oct 1, 2009 by Vasilij Savin <vasilij.savin@gmail.com>
%%%-------------------------------------------------------------------

-module(master_node).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    chronicler:info("Master application started~n"),
    case master_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% @doc
%% All arguments are ignored by now.
%% Stops application and logs shutdown.
%% @end
%% Returns: 'ok'
%% --------------------------------------------------------------------
stop(_) ->
%%     logger ! {event, self(), 
%%         io_lib:format("Master node is shutting down...", [])},
    ok.
