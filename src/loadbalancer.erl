%%%-------------------------------------------------------------------
%%% @author Burbas
%%% @doc
%%% Loadbalancer - Balances load between master nodes
%%% @end
%%% Created : 5 Okt 2009 by Burbas
%%%-------------------------------------------------------------------
-module(loadbalancer).
-export([start_link/1, stop/0, init/1, handle_call/3]).
-export([code_change/3, handle_cast/2, terminate/2, handle_info/2]).
-behaviour(gen_server).
-include("loadbalancer.hrl").
-vsn('$Rev$').


start_link(Nodes) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Nodes], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% API

loadbalancer_loop() ->
    receive 
        % Some request from node
        {request, task, PID} ->
            % Spawn a relay-station to dispatcher
            gen_server:call(?MODULE, {task_request, PID});
        {taskFinished, PID} ->
            % Talk to the job-girl and ask what she want to do
            gen_server:call(?MODULE, {task_finished, PID});
        {taskAbort, PID} ->
            % Talk to the dispatcher
            gen_server:call(?MODULE, {task_abort, PID});
        {addMaster, PID} ->
            % Put it in the TddaskDispatcherList
            gen_server:call(?MODULE, {add_master, PID});
        {removeMaster, PID} ->
            % Remove the PID masternode from list (Expensive)
            gen_server:call(?MODULE, {remove_master, PID})
    end.

init(Nodes) ->
    mnesia:create_schema([node()|Nodes]),
    mnesia:start(),
    {atomic, ok} = mnesia:create_table(master_nodes, 
            [{type, ordered_set}, 
            {attributes, record_info(fields, master_nodes)}]),
    {ok, init}.


handle_call({stop}, _From, _LoopData) ->
    {ok, normal, null};
handle_call({add_master, PID}, _From, _LoopData) ->
    mnesia:write(#master_nodes{load = 0, master_pid = PID});
handle_call({remove_master, PID}, _From, _LoopData) ->
    Delete = #master_nodes{load='_', master_pid = PID},
    mnesia:delete(Delete);
handle_call({task_abort, PID}, _From, _LoopData) ->
    % Find the master with lowest load
    {atomic, [_Load, MasterPID]} = mnesia:first(master_nodes),
    % Sends a message to the dispatcher located on the master specified above
    MasterPID ! {task_request, PID};
handle_call({task_finished, PID}, _From, _LoopData) ->
    % Notify the dispatcher
    {atomic, [_Load, MasterPID]} = mnesia:first(master_nodes),
    MasterPID ! {task_finished, PID}.

code_change(_OldVersion,State,_Extra) ->
    {ok, State}.

handle_cast(_,_) ->
    {ok}.

terminate(normal,_State) ->
    mnesia:delete_object(master_nodes),
    {ok}.

handle_info(_Message,State) ->
    {noreply, State}.
