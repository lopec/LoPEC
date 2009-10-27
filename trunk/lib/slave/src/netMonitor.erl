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
%% Asks the system about how much data that has been sent/received
%% on the network interface card.
%%
%% @spec get_net_stats() -> {Up, Down}
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
            {Up, Down} = {0, 0}
    end,
    {Up, Down}.
