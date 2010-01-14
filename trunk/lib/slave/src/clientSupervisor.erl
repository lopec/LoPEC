%%%-------------------------------------------------------------------
%%% @private
%%% @author Fredrik Andersson <sedrik@consbox.se>
%%% @copyright (C) 2009, Clusterbusters
%%% @doc The client supervisor supervises the client supervision tree
%%% @end
%%% Created : 29 Sep 2009 by Fredrik Andersson <sedrik@consbox.se>
%%%-------------------------------------------------------------------
-module(clientSupervisor).
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
    chronicler:info("~w : module started~n", [?MODULE]),
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
	 [child(dynamicSupervisor, supervisor, no_args),
          child(statistician, worker, [slave]),
	  child(taskFetcher, worker, []),
          child(io_module, worker,
                case configparser:read_config("/etc/lopec.conf",
                                              storage_backend) of
                         {ok, fs} -> [fs_io_module, no_args];
                         {ok, riak} -> [riak_io_module,
                                        [{riak_node,
                                          list_to_atom("riak@"
                                                       ++ os:getenv("MYIP"))}]]
                end)
	 ]
	}
    }.

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
    {Module, {Module, start_link, Args},
        permanent, brutal_kill, Role, [Module]}.
