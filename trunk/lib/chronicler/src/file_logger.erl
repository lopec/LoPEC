%%%-------------------------------------------------------------------
%%% @private
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc the file_logger is an event handler that will print the logging
%%% messages to a log file.
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(file_logger).
-behaviour(gen_event).

-record(state, {logFile, tty = []}).

-export([init/1,
        handle_event/2,
        terminate/2,
        handle_call/2,
        handle_info/2,
        code_change/3
    ]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, LogDir} =
    configparser:read_config("/etc/clusterbusters.conf",
        log_dir),
    LogFile = LogDir ++ "/" ++ atom_to_list(node()),

    {ok, Log} = file:open(LogFile, [append]),

    State =  #state{logFile = Log},

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% The form of the error message can be any erlang term that is accepted by
%% io:format but it is recommended that it is a simple string.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State}
%% @end
%%--------------------------------------------------------------------
handle_event(Msg, State) ->
    process_message(Msg, State),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    %close the logfile
    error_logger:logfile(close),

    chronicler:info("~w:Received terminate call.~n"
        "Reason: ~p~n",
        [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prints the message to the logfile in State
%% @spec process_message(Message) -> ok
%% @end
%%--------------------------------------------------------------------
process_message({_, _, {_, Type, Msg}}, State) when
Type =:= lopec_info;
Type =:= lopec_debug;
Type =:= lopec_user_info;
Type =:= lopec_error;
Type =:= lopec_warning ->
    {time, {{Year,Month,Day},{Hour,Minute,Second}}} = lists:keyfind(time, 1, Msg),
    {message, Message} = lists:keyfind(message, 1, Msg),

    %Write to file
    io:format(State#state.logFile, "~n"
        "=== ~p === ~B/~B-~B = ~B:~B.~B ==~n",
        [Type, Day, Month, Year, Hour, Minute, Second]),
    io:format(State#state.logFile, "Message: ~p~n", [Message]),

    %TTY TODO: add filter for this.
    io:format("~n"
        "=== ~p === ~B/~B-~B = ~B:~B.~B ==~n",
        [Type, Day, Month, Year, Hour, Minute, Second]),
    io:format("Message: ~p~n", [Message]),

    ok;
process_message(_, State) -> %Not supported message type, discard it.
    ok.
