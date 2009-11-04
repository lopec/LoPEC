%%%-------------------------------------------------------------------
%%% @author Bjoern Dahlman <>
%%% @copyright (C) 2009, Bjoern Dahlman
%%% @doc
%%% Fetches information about the network statistics on the system.
%%% @end
%%% Created : 26 Oct 2009 by Bjoern Dahlman <>
%%%-------------------------------------------------------------------
-module(netMonitor).

%% API
-export([get_net_stats/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Asks the system how much data has been sent/received on a network
%% interface card (hardcoded as en0/eth0).
%%
%% @spec get_net_stats() -> {Up, Down}
%% @todo make get_net_stats() paramterized by interface name,
%%       keep it in the config file
%% @end
%%--------------------------------------------------------------------

get_net_stats() ->
    case os:cmd("uname") -- "\n" of
        "Darwin" ->
            A = lists:nth(2, string:tokens(os:cmd("netstat -I en0 -b"), "\n")),
            B = string:tokens(A, " "),
            {Up, Down} = {list_to_integer(lists:nth(10, B)),
                list_to_integer(lists:nth(7, B))};
        "Linux" ->
            A = string:tokens(os:cmd("/sbin/ifconfig eth0 | grep bytes"), ":"),
            Up = list_to_integer(hd(string:tokens(lists:nth(2, A), " "))),
            Down = list_to_integer(hd(string:tokens(lists:nth(3, A), " ")));
        _ ->
	    chronicler:debug("~w : get_net_stats() called on unsupported OS~n"),
	    {Up, Down} = {olol, din_dummis}
    end,
    {Up, Down}.
