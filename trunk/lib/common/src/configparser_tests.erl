%%%-------------------------------------------------------------------
%%% @author Burbas
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(configparser_tests).

-include_lib("eunit/include/eunit.hrl").


configparser_test_() ->
    {setup,
     fun configparser:start_link/0,
     fun (_) -> configparser:stop() end,
     fun (_) ->
             {inparallel,
              [?_assertEqual({error, enoent},
                             configparser:read_config("ysnftf, HEHE", key)),
               ?_assertEqual({ok, "/storage/nfs/me"},
                             configparser:read_config("src/testconfig.conf",
                                                      nfs_mount))]}
     end}.
