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
    fun() ->
        {ok} end,
     fun(_) ->   
        {ok} end,   
        
     fun () ->
         {inorder,
            [
                ?_assertEqual({error, enoent},
                        configparser:read_config("ysnftf, HEHE", key)),
                ?_assertEqual({ok, "/storage/nfs/me"},
                        configparser:read_config("src/testconfig.conf", nfs_mount)),
                ?_assertEqual({ok, "jansson"}, 
                        configparser:parse("Apan", [{"Sven", "Kartofell"}, {"Apan", "jansson"}])),
                ?_assertEqual({error, not_found}, 
                        configparser:parse("Hitman Sedrik", [{"Apan", "jansson"}, {"Peter", "grävling"}])
            ]
          }
    end
    }.
