%%% Author  : Sedrik
%%% Description : The client supervisor
%%% Created : Tue Sep 29 08:58:17 CEST 2009

%-vsn('$Rev$').

-module(logger_ext_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
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
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(WhereToLog) ->
    {ok,{{one_for_one, 1, 60},
            [child(logger_ext, WhereToLog)]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Simple helper function to make the child specefication list easier
%% to read.
%%
%% @spec child(Module, Args) -> {ChildSpec}
%% @end
%%--------------------------------------------------------------------
child(Module, Args) ->
    {Module, {Module, start_link, [Args]}, permanent, brutal_kill, worker, [Module]}.
