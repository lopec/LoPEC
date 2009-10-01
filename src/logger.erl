%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The logger supervisor
%%% Supervises the logging supervision tree
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(logger).
-behaviour(supervisor).

%% API
-export([start_link/0, logMsg/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, no_args).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications. It also starts an event manager named logger
%% and registers the terminalLogger to this manager.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(no_args) ->
    gen_event:start_link({local, logger}),
%    gen_event:add_handler(logger, terminalLogger, []),
    gen_event:add_handler(logger, fileLogger, [logfile]),
    {ok,{{one_for_one, 1, 60}, []}}.


logMsg(Msg) ->
    gen_event:notify(logger, Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
