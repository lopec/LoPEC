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

-module(riak_io_module).

%% API
-export([init/0, put/4, get/3]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the storage. Loads path where data will be stored.
%%
%% @spec init(Args::list()) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init() ->
    {ok, {riak, C}} = riak:client_connect("riaknode@" ++ os:get_env("MYIP")).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Puts a value to the storage, either the file system or riak
%% depending on how the server was started.
%%
%% @spec put(Bucket, Key, Val) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
put(Bucket, Key, Value, State = {riak, Ref}) ->
    Object = riak_object:new(Bucket, Key, Value),
    Reply = Ref:put(Object, 1).
    
%%--------------------------------------------------------------------
%% @doc
%% Gets the value associated with the bucket and the key.
%%
%% @spec get(Bucket, Key) -> binary() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get(Bucket, Key, State = {riak, Ref}) ->
    {ok, Object} = Ref:get(Bucket, Key, 1),
    Reply = riak_object:get_value(Object).

