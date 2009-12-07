%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Fredrik Andersson
%%% @doc
%%% main_chronicler is responsible for keeping a database over the logging
%%% messages passed to the system. It runs on the node logger only and
%%% should only be runned once since it is globaly registered.
%%% @end
%%% Created : 02 Dec 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(main_chronicler).
-behaviour(gen_server).

-define(LOG_TABLE, log_table).

-record(state, {ets_table}).
-record(log_message,
    {
        type,
        user = anonymous,
        fromNode,
        time,
        message = []
    }).

%% API
-export([
        start_link/0,
        get_everything/0
    ]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the master chronicler that holds a database over the log messages in
%% the system.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, no_args, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns all the log messages in the database
%% @spec get_everything() -> {ok, Match}
%% @end
%%--------------------------------------------------------------------
get_everything() ->
    Reply = gen_server:call({global, ?MODULE}, {request, get_everything}),
    Reply.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(no_args) ->
    %global:register_name(externalLoggerPID, self()),

    EtsTable = ets:new(?LOG_TABLE,
        [duplicate_bag, protected, named_table,
            {keypos, 2}, {heir, none},
            {write_concurrency, false}]),

    State = #state{ets_table = EtsTable},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the whole database
%% @spec handle_call({request, get_everything}, From, State) -> {reply, Match}
%% @end
%%--------------------------------------------------------------------
handle_call({request, get_everything}, _From, State) ->
    Match = ets:match(log_table,
        #log_message{
            type = '$1',
            user = '$2',
            fromNode = '$3',
            time = '$4',
            message = '$5'
        }),
    {reply, Match, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:debug("~w: Received unexpected handle_call from ~p.~n"
        "Msg: ~p~n",
        [?MODULE, From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling casted messages, checks to see if Level is in the logging levels of
%% state
%%
%% @spec handle_cast(Msg, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    process_message(Msg),
    {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
%handle_cast(Msg, State) ->
%    chronicler:debug("~w: Received unexpected handle_cast.~n"
%                       "Msg: ~p~n",
%                       [?MODULE, Msg]),
%    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    chronicler:debug("~w:Received unexpected handle_info.~n"
        "Info: ~p~n",
        [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    chronicler:debug("~w: Received terminate call.~n"
        "Reason: ~p~n",
        [?MODULE, Reason]),
    ets:delete(State#state.ets_table),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% Logs and discards unexpected messages.
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    chronicler:debug("~w: Received code_change call.~n"
        "Old version: ~p~n"
        "Extra: ~p~n",
        [?MODULE, OldVsn, Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores a message in the local database
%% @spec process_message(Message) -> ok
%% @end
%%--------------------------------------------------------------------
process_message({{Node, From}, {_,_,{_, Type, Msg}}}) when
Type =:= lopec_info;
Type =:= lopec_debug;
Type =:= lopec_user_info;
Type =:= lopec_error;
Type =:= lopec_warning ->

    {time, Time} = lists:keyfind(time, 1, Msg),

    User =
    case lists:keyfind(user, 1, Msg) of
        false -> anonymous;
        {user, U} -> U
    end,

    {message, Message} = lists:keyfind(message, 1, Msg),

    ets:insert(?LOG_TABLE,
        #log_message{
            type = Type,
            user = User,
            fromNode = Node,
            time = Time,
            message = Message
        }),
    ok;
process_message(Msg) ->
    ok.
