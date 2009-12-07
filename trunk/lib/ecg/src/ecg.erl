%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <vasilij.savin@gmail.com>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% This module handles start of electrocardiogram application.
%%% Currently it is not using any parameters.
%%% @end
%%% Created : Oct 5, 2009 by Vasilij Savin 
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
    chronicler:info("~w : Starting ECG Application~n", [?MODULE]),
    Result = ecg_sup:start_link(),
    case Result of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    chronicler:error(fix_me_need_user_id, "~w : ECG Application StartUp Error: ~p ~n",
                             [?MODULE, Error]),
	    Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.
