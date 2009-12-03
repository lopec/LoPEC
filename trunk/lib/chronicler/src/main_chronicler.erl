%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Fredrik Andersson
%%% @doc
%%%
%%% main_chronicler is responsible for keeping a database over the logging
%%% messages passed to the system. It runs on the node logger@<host> only and
%%% should only be runned once since it is globaly registered.
%%%
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
        fromNode,
        message = ""
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
%% @spec start_link(Type) -> {ok, Pid} | ignore | {error, Error}
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
%% @spec handle_call({request, get_everything}, State) -> {noreply, State} |
%% @end
%%--------------------------------------------------------------------
handle_call({request, get_everything}, _From, State) ->
    Match = ets:match(log_table,
        #log_message{type = '$1', fromNode = '$2', message = '$3'}),
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
    chronicler:warning("~w: Received unexpected handle_call from ~p.~n"
        "Msg: ~p~n",
        [?MODULE, From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling casted messages, checks to see if Level is in the logging levels of
%% state
%%
%% @spec handle_cast(Level, State) -> {noreply, State} |
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("Got message: ~p~n", [Msg]),
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
%    chronicler:warning("~w: Received unexpected handle_cast.~n"
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
    chronicler:warning("~w:Received unexpected handle_info.~n"
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
%% Messages are recieved on the form.
%% {info, <- error_logger type (info, warning or error)
%% {slogger@localhost, <9518.4.0>}, <- (node(), pid of externalLogger
%% {<9518.42.0>, <- the pid that sent the event
%%   std_info, <- custom type if called ass error_logger:info_report(custom_type, msg)
%%   "Node slogger@localhost transmitting stats.\n"} <- The message
%}
%%
%% @spec process_message(Message) -> ok
%% @end
%%--------------------------------------------------------------------
process_message({_, {Node, _}, {_, lopec_info, Msg}}) ->
    ets:insert(?LOG_TABLE,
        #log_message{
            type = lopec_info,
            fromNode = Node,
            message = Msg
        }),
    ok;
process_message({_, {Node, _}, {_, lopec_debug, Msg}}) ->
    ets:insert(?LOG_TABLE,
        #log_message{
            type = lopec_debug,
            fromNode = Node,
            message = Msg
        }),
    ok;
process_message({_, {Node, _}, {_, lopec_user_info, Msg}}) ->
    ets:insert(?LOG_TABLE,
        #log_message{
            type = lopec_user_info,
            fromNode = Node,
            message = Msg
        }),
    ok;
process_message({_, {Node, _}, {_, lopec_error, Msg}}) ->
    ets:insert(?LOG_TABLE,
        #log_message{
            type = lopec_error,
            fromNode = Node,
            message = Msg
        }),
    ok;
process_message({_, {Node, _}, {_, lopec_warning, Msg}}) ->
    ets:insert(?LOG_TABLE,
        #log_message{
            type = lopec_warning,
            fromNode = Node,
            message = Msg
        }),
    ok;
process_message(_) ->
    ok.
