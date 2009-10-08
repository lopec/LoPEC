%%%-------------------------------------------------------------------
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The logger supervisor
%%% Supervises the logging supervision tree
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(logger_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

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
%% specifications. It also starts an event manager named
%% logger_manager and registers the terminalLogger to this manager.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(no_args) ->
    %gen_event:start_link({local, logger}),
    {ok,{{rest_for_one, 1, 60},
            [   child(gen_event, worker, {local, logger_manager}),
                child(logger, worker, logger_manager)
            ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Simple helper function to make the child specification list easier
%% to read.
%%
%% @spec child(Module, Role, Args) -> {ChildSpec}
%% @end
%%--------------------------------------------------------------------
child(Module,supervisor, no_args) ->
    {Module, {Module, start_link, []},
        permanent, infinity, supervisor, [Module]};
child(Module,Role, no_args) ->
    {Module, {Module, start_link, []},
        permanent, brutal_kill, Role, [Module]};
child(Module, Role, Args) ->
    {Module, {Module, start_link, [Args]},
        permanent, brutal_kill, Role, [Module]}.
