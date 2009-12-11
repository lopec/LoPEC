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
-export([init/1, put/4, get/3]).

-record(state, {client, reads = 1, wreqs = 1, writes = 1}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the storage. Loads path where data will be stored.
%%
%% @spec init(Args::list()) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    case proplists:lookup(riak_node, Args) of
        {riak_node, RiakNode} ->
            {ok, C} = riak:client_connect(RiakNode),
            case [proplists:lookup(Setting, Args)
                  || Setting <- [riak_reads, riak_wreqs, riak_writes]] of
                [{riak_reads, Reads},
                 {riak_wreqs, WRequests}, {riak_writes, Writes}] ->
                    {ok, {riak, #state{client = C, reads = Reads,
                                       wreqs = WRequests, writes = Writes}}};
                _ ->
                    {ok, {riak, #state{client = C}}}
            end
    end.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Puts a value to the storage, either the file system or riak
%% depending on how the server was started.
%%
%% @spec put(Bucket, Key, Val, State) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
put(Bucket, Key, Value,
    {riak, #state{client = Client, wreqs = WRequests, writes = Writes}}) ->
    Object = riak_object:new(Bucket, Key, Value),
    _Reply = Client:put(Object, WRequests, Writes).
    
%%--------------------------------------------------------------------
%% @doc
%% Gets the value associated with the bucket and the key.
%%
%% @spec get(Bucket, Key, State) -> binary() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get(Bucket, Key,
    {riak, #state{client = Client, reads = Reads}}) ->
    {ok, Object} = Client:get(Bucket, Key, Reads),
    _Reply = {ok, riak_object:get_value(Object)}.
