%%%-------------------------------------------------------------------
%%% @author Vasilij Savin, Gustav Simonsson <>
%%% @doc
%%% ElectroCardioGram - process that keeps track of alive nodes
%%% @end
%%% Created : 29 Sep 2009 by Vasilij Savin <>
%%%-------------------------------------------------------------------
-module(ecg_server).
-behaviour(gen_server).
-revision('$Rev$').
-created_by('Vasilij Savin, Gustav Simonsson').
-author("Vasilij Savin, Gustav Simonsson").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         accept_message/1, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

accept_message(Msg) ->
    logger ! {event, self(),
        io_lib:format("Msg received: ~w", [Msg])},
    gen_server:cast(?MODULE, Msg).

%%%===================================================================
%%% Interface Function
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% gen_server callback function.
%% 
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
%% @public
%% @doc
%% Boots up ECG - cluster heartbeat listener.
%% IMPORTANT: 'logger' should be registered process, otherwise
%% ECG will fail.
%% 
%% @end
%%--------------------------------------------------------------------
init(_) ->
    net_kernel:monitor_nodes(true),
    logger ! {event, self(), "ECG is up and running!"},
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, _State) ->
    Reply = ok,
    {reply, Reply, []}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({nodeup, Node}, _) ->
    logger ! {event, self(), 
        io_lib:format("Welcome new node: ~w", [Node])},
    {noreply, []};
handle_cast({nodedown, Node}, _) ->
    % Stub needed to contact Task List API
    % tasklist:free_tasks(Node),
    logger ! {event, self(),
        io_lib:format("Node ~w just died. :()~n", [Node])},
    {noreply, []};
%% We need to establish connection to new node, if not yet connected
%% This might be obsolete later, depending on comm protocol
handle_cast({new_node, Node}, _) ->
    logger ! {event, self(), 
        io_lib:format("New Node", [])},
    case lists:member(Node, nodes()) of
        false ->
            net_adm:ping(Node);
        true ->
            ok
    end,
    {noreply, []};
handle_cast(UnrecognisedMessage, _) ->
    logger ! {event, self(), 
        io_lib:format("UnrecognisedMessage: ~w", [UnrecognisedMessage])},
    {noreply, []}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, _State) ->
    {noreply, []}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    {ok, []}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

