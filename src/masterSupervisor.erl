%%%-------------------------------------------------------------------
%%% @author Vasilij Savin, Gustav Simonsson <>
%%% @doc
%%% Master supervisor supervises WPM processes.
%%% Currently there are 3 processes to monitor:
%%% Listener   - listen to job submissions from users
%%% DbDaemon   - interaction with DB
%%% Dispatcher - listens to task requests from nodes  
%%% @end
%%% Created : Oct 2, 2009 by Vasilij Savin <> 
%%% -------------------------------------------------------------------
-module(masterSupervisor).
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
    io:format("Starting Supervisor~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupervisorConfig,  [ChildSpec]}} |
%%          ignore                                  |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(no_args) ->
    % Children specification:
    % Process name
    % {Module, Func, Args} - Start function
    % RestartType
    % ShutdownTime
    % Type: worker | supervisor
    % Used modules
    io:format("Creating children~n", []),
    Dispatcher = { dispatcher, 
                   {dispatcher, start_link, []}, 
                   permanent,
                   20,
                   worker,
                   [dispatcher]},
    % Other processes are not yet implemented
%%     Listener = { dispatcher,
%%                    {dispatcher, start_link,[]}, 
%%                    permanent,
%%                    20,
%%                    worker,
%%                    [dispatcher]},
%%     DbDaemon = { dispatcher,
%%                    {dispatcher, start_link,[]}, 
%%                    permanent,
%%                    20,
%%                    worker,
%%                    [dispatcher]},
    
    % Returning supervisor specification
    {ok,{{one_for_one,1,60}, [Dispatcher]}}.