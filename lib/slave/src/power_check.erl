%%%-------------------------------------------------------------------
%%% @private
%%% @author Burbas
%%% @copyright (C) 2009, Clusterbusters
%%% @doc Measure power consumption
%%% @end
%%% Created : 23 Oct 2009 by Burbas
%%%-------------------------------------------------------------------
-module(power_check).
-export([get_load/1, get_watt/1, get_watt_per_task/1]).

%%--------------------------------------------------------------------
%% @doc
%% Ask the system what load it have.
%%
%% @spec get_load(Period) -> Load
%% @end
%%--------------------------------------------------------------------
get_load(Period) ->
    D = os:cmd("uptime") -- "\n",
    Avg = lists:reverse(hd(string:tokens(lists:reverse(D), ":"))),
    %% Checks if it's a Linux or Mac OSX system running
    case os:cmd("uname") -- "\n" of
        "Darwin" -> {ok, [L1, L5, L15], _} = io_lib:fread("~f ~f ~f", Avg);
        "Linux" -> {ok, [L1, L5, L15], _} = io_lib:fread("~f, ~f, ~f", Avg);
        _ -> L1 = 0, L5 = 0, L15 = 0
    end,
    case Period of
        l1min -> L1;
        l5min -> L5;
        l15min -> L15
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get the expected power consumption for a period of time (l1min, l5min
%% and l15min is valid values)
%%
%% @spec get_watt(Period) -> Watt
%% @end
%%--------------------------------------------------------------------
get_watt(Period) ->
    Load = get_load(Period),
    {ok, Cores} = configparser:read_config("/etc/clusterbusters.conf", cores),
    %% This is not a very effective way to measure watt. But it's the only way right know.
    case Load of 
        Load when Load > Cores ->
            {ok, HLW} = configparser:read_config("/etc/clusterbusters.conf", high_load_watt),
            HLW;
        _ -> 
            {ok, HLW} = configparser:read_config("/etc/clusterbusters.conf", high_load_watt),
            {ok, LLW} = configparser:read_config("/etc/clusterbusters.conf", low_load_watt),
            ((HLW-LLW)*(Load/Cores))+LLW
    end.
    

%%--------------------------------------------------------------------
%% @doc
%% This measures power consumptions over a time T where T is expressed
%% in seconds. 
%%
%% @spec get_watt_per_task(Period) -> Watt
%% @end
%%--------------------------------------------------------------------
get_watt_per_task(Period) ->
    Watt = get_watt(l1min),
    Watt*(Period/60).
    
