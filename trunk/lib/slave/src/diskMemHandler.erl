-module(diskMemHandler).

%% API 
-export([start/1, stop/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%-behavior(gen_event).

start(Options) ->
    % gen_event:add_handler/2 doesn't check for duplicates
    case lists:member(?MODULE, gen_event:which_handlers(alarm_handler)) of
	true  ->
	    already_started;
	false ->
	    
	    %todo: change swap to add so we have 2 handlers
	    %case gen_event:swap_sup_handler(alarm_handler,
	    %			{alarm_handler, swap}, {?MODULE, Options})
	    
	    case gen_event:add_sup_handler(alarm_handler, ?MODULE, Options) of
		ok -> 
		    ok;
		{'EXIT',Reason} ->
		    throw({error, {?MODULE, start_link, Reason}});
		Other ->
		    throw({other, {?MODULE, start_link, Other}})
	    end
    end.

stop() ->
    gen_event:swap_handler(alarm_handler, {?MODULE, swap}, {alarm_handler, []}).

%% init/1 is called when a event is being installed to an event manager
%% using gen_event:add_[sup_]handler/3 function
init({_Options, OldAlarms}) ->
    {ok, OldAlarms}.

handle_event({Type, Alarm}, Alarms) when Type=:=set_alarm; Type=:= clear_alarm ->
    log_alarm(Type, Alarm),
    {ok, newState};

handle_event(_, Alarms)->
    {ok, Alarms}.

handle_call(_Query, Alarms) -> {ok, {error, bad_query}, Alarms}.
handle_info(_, Alarms)      -> {ok, Alarms}.

%% terminate/2 is called when
%% gen_event:swap_handler(EventMgr, {?MODULE, swap}, {NewModule, Args}) is invoked
terminate(swap,    Alarms)  -> {?MODULE, Alarms};
terminate(_Reason,_Alarms)  -> ok.

log_alarm(Type, Alarm) when Type==set_alarm; Type==clear_alarm ->
    % do custom logging here
    gen_server:cast({global, statistician}, {alarm, node(), Type, Alarm}),
    io:format("Custom alarm log function invoked for ~w: ~p~n", [Type, Alarm]),
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
    chronicler:debug("~w:Received code_change call.~n"
        "Old version: ~p~n"
        "Extra: ~p~n",
        [?MODULE, OldVsn, Extra]),
    {ok, State}.
