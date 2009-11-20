%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <bjorn.dahlman@gmail.com>
%%% @copyright (C) 2009, Bjorn Dahlman
%%% @doc
%%%
%%% Library containing convenient things for the admin to use.
%%%
%%% @end
%%% Created : 20 Nov 2009 by Bjorn Dahlman <>
%%%-------------------------------------------------------------------
-module(utils).

-include("env.hrl").

%% API
-export([get_cluster_path/0, get_programs/0, get_program_executables/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Gets the path to the cluster from the config file defined in
%% env.hrl
%% @spec get_cluster_path() -> Path::string()
%% @end
%%--------------------------------------------------------------------
get_cluster_path() ->
    {ok, Root} = configparser:read_config(?CONFIGFILE, cluster_root),
    Root.

%%--------------------------------------------------------------------
%% @doc
%% Fetches a list of programs in the cluster.
%% @spec get_programs() -> Programs::list()
%% @end
%%--------------------------------------------------------------------
get_programs() ->
    {ok, Result} = file:list_dir(
                     lists:concat([get_cluster_path(), "programs/"])),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% Checks what executables are available for a specific program.
%% @spec get_program_executables(Program::term()) -> Executables::list()
%% @end
%%--------------------------------------------------------------------
get_program_executables(Program) ->
    {ok, Result} = file:list_dir(
                     lists:concat([get_cluster_path(), "/programs/", Program])),
    Result.
