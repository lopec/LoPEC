%%%-------------------------------------------------------------------
%%% @author Vasilij Savin <>
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% ECG supervisor - watches a single worker, ECG server. 
%%% @end
%%% Created : Oct 5, 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------

-module(ecg_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(no_args) ->
    % Children specification:
    % Process name
    % {Module, Func, Args} - Start function
    % RestartType
    % ShutdownTime
    % Type: worker | supervisor
    % Used modules
    io:format("Creating ECG Child Specification~n", []),
    ECG = {  ecg_server,
             {ecg_server,start_link,[]},
             permanent,
             20,
             worker,
             [ecg_server]
          },
    
    % Returning supervisor specification
    %io:format("Send ECG Specification ~p ~n", [ECG]),
    {ok,{{one_for_one,1,10}, [ECG]}}.
