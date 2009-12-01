%%%-------------------------------------------------------------------
%%% @author Henkan <henkethalin@hotmail.com>
%%% @author Gustav Simonsson <gusi7871@student.uu.se>
%%% @doc
%%%
%%% Custom event handler, adds itself to SASL event manager
%%% 'alarm_handler' and sends any alarm event receieved to
%%% global statistician, as a cast in log_alarm.
%%%
%%% @end
%%% Created : Nov 2009 by Gustav Simonsson <gusi7871@student.uu.se>
%%%-------------------------------------------------------------------

-module(diskMemHandler).

%% API
-export([start/0, stop/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
%%--------------------------------------------------------------------
%% @doc
%%
%% Adds itself to SASL event manager alarm_handler if not already
%% added to it. add_sup_handler is used as gen_event:add_handler/2
%% doesn't check for duplicates.
%%
%% @spec stop() -> ok | {'EXIT',Reason} | Other
%% @end
%%--------------------------------------------------------------------
start() ->
    case lists:member(?MODULE, gen_event:which_handlers(alarm_handler)) of
	true  ->
	    already_started;
	false ->
	    case gen_event:add_sup_handler(alarm_handler, ?MODULE, no_args) of
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
init(no_args) ->
    {ok, []}.

handle_event({Type, Alarm}, Alarms) when Type == set_alarm; Type == clear_alarm ->
    log_alarm(Type, Alarm),
    {ok, Alarms};

handle_event(_, Alarms)->
    {ok, Alarms}.

handle_call(_Query, Alarms) -> {ok, {error, bad_query}, Alarms}.
handle_info(_, Alarms)      -> {ok, Alarms}.

%% terminate/2 is called when
%% gen_event:swap_handler(EventMgr, {?MODULE, swap}, {NewModule, Args}) is invoked
terminate(swap,    Alarms)  -> {?MODULE, Alarms};
terminate(_Reason,_Alarms)  -> ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called when diskMemHandler as attached event_handler receives an
%% event which is either set_alarm or clear_alarm. Propagates the
%% alarm to global statistician.
%%
%% @spec log_alarm(Type, Alarm) -> ok
%% @end
%%--------------------------------------------------------------------
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
