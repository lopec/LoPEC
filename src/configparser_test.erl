%%%-------------------------------------------------------------------
%%% @author Burbas
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(configparser_test).

-include_lib("eunit/include/eunit.hrl").


configparser_test() ->
    configparser:start_link(),
    {error, enoent} = configparser:read_config("you should not find this file.HEHE", key),
    {ok, "/storage/nfs/me"} = configparser:read_config("testconfig.conf", nfs_mount),
    {test_success}.

