%%%-------------------------------------------------------------------
%%% @private
%%% @author Burbas
%%% @copyright (C) 2009, Clusterbusters
%%% @doc Measure power consumption
%%% @end
%%% Created : 23 Oct 2009 by Burbas
%%%-------------------------------------------------------------------

-module(power_check).
-include("../include/env.hrl").

-export([get_load/1, get_watt/1, get_watt_per_task/1]).

%%--------------------------------------------------------------------
%% @doc
%% Ask the system what load it has.
%%
%% @spec get_load(Period) -> Load
%% @end
%%--------------------------------------------------------------------
get_load(Period) ->
    cpu_sup:start(),
    case Period of
        l1min -> T = avg1;
        l5min -> T = avg5;
        l15min -> T = avg15
    end,
    cpu_sup:T()/256.

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
    {ok, Cores} = configparser:read_config(?CONFIGFILE, cores),
    case Period of
        l1min -> T = 60;
        l5min -> T = 5*60;
        l15min -> T = 15*60
    end, 
    %% This is not a very effective way to measure watt. But it's the only way right know.
    {ok, HLW} =
        configparser:read_config(?CONFIGFILE, high_load_watt),
    {ok, LLW} =
        configparser:read_config(?CONFIGFILE, low_load_watt),
    case Load of 
        Load when Load > Cores ->
            HLW;
        _ -> 
            (((HLW-LLW)*(Load/Cores))+LLW)*T
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
    
