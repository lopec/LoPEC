%%%-------------------------------------------------------------------
%%% @author Vasilij Savin
%%% @copyright (C) 2009, Vasilij Savin
%%% @doc
%%% Deals with the temporary storage in the cluster. Gets a binary
%%% stream of data to write or returns the binary stream of data.
%%%
%%% @end
%%% Created :  2 Dec 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------
-module(fs_io_module).

%% API
-export([init/1, put/4, get/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the storage. Loads path where data will be stored.
%%
%% @spec init(Args::list()) -> {ok, State} |
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    {ok, Path} = configparser:read_config("/etc/clusterbusters.conf",
                                          cluster_root),
    {ok, {fs, Path}}.
    
%%--------------------------------------------------------------------
%% @doc
%% Puts a value to the storage, either the file system or riak
%% depending on how the server was started.
%%
%% @spec put(Bucket, Key, Val) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
put(Bucket, Key, Value, State = {fs, Path}) ->
    Filename = lists:concat([Path, "tmp/", Bucket, "/", Key]),
    filelib:ensure_dir(Filename),
    Reply = file:write_file(Filename, Value).
    
%%--------------------------------------------------------------------
%% @doc
%% Gets the value associated with the bucket and the key.
%%
%% @spec get(Bucket, Key) -> {ok, binary()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get(Bucket, Key, State = {fs, Path}) ->
    Filename = lists:concat([Path, "tmp/", Bucket, "/", Key]),
    {ok, Data} = file:read_file(Filename).
