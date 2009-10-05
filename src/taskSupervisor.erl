%%%-------------------------------------------------------------------
%%% @private
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The taskSupervisor supervises the taskFetcher
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(taskSupervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications. It also starts an event manager named error_manager
%% and registers the terminalLogger to this manager.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(no_args) ->
    {ok,{{one_for_one, 1, 60},
            [   child(taskFetcher, worker, [taskFetcher_tests]) %TODO change this to server address!
            ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Simple helper function to make the child specefication list easier
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
    {Module, {Module, start_link, Args},
        permanent, brutal_kill, Role, [Module]}.
