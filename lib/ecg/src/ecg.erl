%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% This module handles start of electrocardiogram application.
%%% Currently it is not using any parameters.
%%% @end
%%% Created : Oct 5, 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------

-module(ecg).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%           Error
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    chronicler:info(io_lib:format("Started ECG Application~n", [])),
    Result = ecg_sup:start_link(),
    case Result of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    chronicler:info(io_lib:format
			    ("ECG Application StartUp Error: ~p ~n",
			     [Error])),
	    Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.
