%%%-------------------------------------------------------------------
%%% @author Burbas
%%% @doc
%%% Loadbalancer - Balances load between master nodes
%%% @end
%%% Created : 5 Okt 2009 by Burbas
%%%-------------------------------------------------------------------
-module(loadbalancer).
-export([start_link/0, stop/0, init/1, handle_call/3]).
-export([code_change/3, handle_cast/2, terminate/2, handle_info/2]).
-export([request_task/1, task_abort/1, task_finished/1, add_master/1, remove_master/1]).
-export([create_schema/1]).
-behaviour(gen_server).
-include("loadbalancer.hrl").
-vsn('$Rev$').


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the genserver
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Stops the genserver
%%
%% @spec request_task(PID) -> void()
%% @end
%%--------------------------------------------------------------------
request_task(PID) ->
    gen_server:call(?MODULE, {task_request, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a master-node that a task on PID have finished.
%%
%% @spec task_finished(PID) -> {ok}
%% @end
%%--------------------------------------------------------------------
task_finished(PID) ->
    gen_server:call(?MODULE, {task_finished, PID}).
%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a master-node that a node wants to abort its 
%% current task.
%%
%% @spec task_abort() -> {ok}
%% @end
%%--------------------------------------------------------------------
task_abort(PID) ->
    gen_server:call(?MODULE, {task_abort, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Adds a master-node to the internal database. When its added it will
%% start to receive request from nodes.
%%
%% @spec add_master(PID) -> {ok}
%% @end
%%--------------------------------------------------------------------
add_master(PID) ->
    gen_server:call(?MODULE, {add_master, PID}).
%%--------------------------------------------------------------------
%% @doc
%% Removes a master from the internal database. 
%%
%% @spec remove_master(PID) -> {ok}
%% @end
%%--------------------------------------------------------------------
remove_master(PID) ->
    gen_server:call(?MODULE, {remove_master, PID}).

%%--------------------------------------------------------------------
%% @doc
%% Creates the mnesia schema.
%%
%% @spec create_schema(Nodes) -> {ok}
%% @end
%%--------------------------------------------------------------------
create_schema(Nodes) ->
    mnesia:create_schema([node()|Nodes]),
    mnesia:start(),
    mnesia:create_table(master_nodes, 
            [{type, ordered_set}, 
            {attributes, record_info(fields, master_nodes)}]),
    {ok}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server. It will distribute the mnesia database to
%% itself and the nodes specified in the argument.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    mnesia:start(),
    mnesia:wait_for_tables([master_nodes], 2000),
    {ok, init}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({stop}, _From, State) ->
    {ok, normal, State};
handle_call({add_master, PID}, _From, State) ->
    F = fun() ->
        Rec = #master_nodes{load = 0, master_pid = PID},
        mnesia:write(Rec)
    end,
    {atomic, ok} = mnesia:transaction(F),
    {reply, ok, State};

handle_call({remove_master, PID}, _From, State) ->
    F = fun() ->
        Delete = #master_nodes{load='_', master_pid = PID},
        Records = mnesia:match_object(Delete),
        lists:foreach(fun mnesia:delete_object/1, Records)
    end,
    {atomic, ok} = mnesia:transaction(F),
    {reply, ok, State};
handle_call({request_task, PID}, _From, State) ->
 % Find the master with lowest load
    F = fun() ->
        Record = mnesia:first(master_nodes)
    end,
    mnesia:transaction(F),
    % Sends a message to the dispatcher located on the master specified above
    %MasterPID ! {task_request, PID},
    {reply, ok, State};
handle_call({task_abort, PID}, _From, State) ->
       {reply, ok, State};
handle_call({task_finished, PID}, _From, State) ->
    % Notify the dispatcher
    {atomic, [_Load, MasterPID]} = mnesia:first(master_nodes),
    MasterPID ! {task_finished, PID},
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVersion,State,_Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_,State) ->
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
terminate(normal,_State) ->
    mnesia:delete_object(master_nodes),
    {ok}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Message,State) ->
    {noreply, State}.
