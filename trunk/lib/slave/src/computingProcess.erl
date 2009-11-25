%%%-------------------------------------------------------------------
%%% @author Bjorn Dahlman <bjorn.dahlman@gmail.com>
%%% @copyright (C) 2009, Clusterbusters
%%% @version 0.0.2
%%% @doc
%%% The erlang process that communicates with the external process
%%% on the node.
%%% @end
%%% Created : 30 Sep 2009 by Bjorn Dahlman <>
%%%-------------------------------------------------------------------

-module(computingProcess).
-include("../include/env.hrl").

-behaviour(gen_server).

%% API
-export([start_link/5, stop/0, stop_job/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

kill_all_procs([]) ->
    [];
kill_all_procs([{pid, Pid}|T]) ->
    chronicler:info("~w : Killing PID = ~w~n", [?MODULE, Pid]),
    os:cmd("kill -9 " ++ Pid),
    kill_all_procs(T);
kill_all_procs([H|T]) ->
    chronicler:info("~w : Killing PID (thru pid-file) = ~w~n", [?MODULE, H]),
    os:cmd("kill -9 " ++ H),
    kill_all_procs(T).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server. Path is the path to the external program, Op
%% is the first argument, Arg1 is the second and Arg2 is the third
%% argument. So the os call will look like "Path Op Arg1 Arg2".
%% The TaskId is there for the statistician.
%%
%% @spec start_link(Path, Op, Arg1, Arg2, TaskId) ->
%%           {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Path, Op, JobId, InputPath, TaskId) ->
    chronicler:info("~w : application starting~n", [?MODULE]),
    StringId = integer_to_list(JobId),
    {ok, Root} = configparser:read_config(?CONFIGFILE, cluster_root),
    {ok, Platform} = configparser:read_config(?CONFIGFILE, platform),
    Prog = Root ++ "programs/" ++ atom_to_list(Path) ++ "/script." ++ Platform,
    LoadPath = Root ++ InputPath,
    SavePath = Root ++
        case Op of
            split -> "tmp/" ++ StringId ++ "/map/";
            map -> "tmp/" ++ StringId ++ "/reduce/";
            reduce -> "tmp/" ++ StringId ++ "/results/";
            finalize ->
                Dir = Root ++ "results/" ++ StringId ++ "/",
                filelib:ensure_dir(Dir),
                file:change_mode(Dir, 8#777),
                "results/" ++ StringId
        end,
    gen_server:start_link({local, ?SERVER},?MODULE,
			  [Path, Prog, atom_to_list(Op),
			   LoadPath, SavePath, JobId, TaskId], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------
stop() ->
    chronicler:info("~w : module stopping~n", [?MODULE]),
    gen_server:cast(?SERVER, stop).


%%--------------------------------------------------------------------
%% @doc
%% Stops the currently running task on the node.
%
%% @spec stop_job() -> ok
%% @end
%%--------------------------------------------------------------------
stop_job() ->
    chronicler:debug("~w : Stopping current task.~n", [?MODULE]),
    gen_server:call(?MODULE, {stop_job}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Progname, Path, "split", LoadPath, SavePath, JobId, TaskId]) ->
    Splits = dispatcher:get_split_amount(),
    chronicler:debug("~w : Path: ~ts~nOperation: ~ts~nLoadpath:"
		     " ~ts~nSavepath: ~ts~nJobId: ~p~n",
                     [?MODULE, Path, "split", LoadPath, SavePath, JobId]),
    open_port({spawn_executable, Path},
	      [use_stdio, stderr_to_stdout, exit_status, {line, 512},
	       {args, ["split", LoadPath, SavePath, integer_to_list(Splits)]}]),
    {ok, {JobId, TaskId, now(), "split", Progname, []}};
init([Progname, Path, Op, LoadPath, SavePath, JobId, TaskId]) ->
    chronicler:debug("~w : Path: ~ts~nOperation: ~ts~nLoadpath:"
		     " ~ts~nSavepath: ~ts~nJobId: ~p~n",
                     [?MODULE, Path, Op, LoadPath, SavePath, JobId]),
    open_port({spawn_executable, Path},
	      [use_stdio, stderr_to_stdout, exit_status, {line, 512},
	       {args, [Op, LoadPath, SavePath]}]),
    {ok, {JobId, TaskId, now(), Op, Progname, []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
%% HERE IS THE EVUL PRÅBLEM
handle_call({stop_job}, _From,
            {JobId, TaskId, Time, TaskType, Progname, StartedPids}) ->
    {ok, Root} = configparser:read_config("/etc/clusterbusters.conf", cluster_root),
    PidPath = Root ++ "/tmp/" ++ integer_to_list(JobId) ++ "/*.pid",
    Result = kill_all_procs(StartedPids),
    PidFiles = os:cmd("for pidFile in `ls " ++ PidPath ++ "` ; do cat $pidFile; echo \"\n\"; done"),
    Tokens = string:tokens(PidFiles, "\n"),
    kill_all_procs(Tokens),
    
    %taskfetcher:reset_state(),
    {noreply, {JobId, TaskId, Time, TaskType, Progname, Result}};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:warning("~w : Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.

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
handle_cast(stop, State) ->
    {stop, normal, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning("~w : Received unexpected handle_cast call.~n"
                       "Message: ~p~n",
                       [?MODULE, Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages. Handles message sent from
%% the program started with open_port in init, logs them and reports
%% new split/reduce/result tasks to the taskFetcher.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({_Pid, {data, {_Flag, "MY_PID " ++ Data}}},
            {JobId, TaskId, Time, TaskType, Progname, StartedPids}) ->
    chronicler:debug("~w : MY_PID ~w Reported~n", [?MODULE, Data]),
    {noreply, {JobId, TaskId, Time, TaskType, Progname, 
        [{pid, Data}|StartedPids]}};
handle_info({_Pid, {data, {_Flag, "NEW_SPLIT " ++ Data}}},
            State = {JobId, TaskId, Time, TaskType, Progname, _StartedPids}) ->
    chronicler:debug("~w : SPLIT: New map task: ~ts~n", [?MODULE, Data]),
    taskFetcher:new_task({JobId, TaskId, Time, TaskType, Progname},
                         map, "/map/" ++ Data),
    {noreply, State};
handle_info({_Pid, {data, {_Flag, "NEW_REDUCE_TASK " ++ Data}}},
            State = {JobId, TaskId, Time, TaskType, Progname, _StartedPids}) ->
    chronicler:debug("~w : MAP: New reduce task: ~ts~n", [?MODULE, Data]),
    taskFetcher:new_task({JobId, TaskId, Time, TaskType, Progname},
                         reduce, "/reduce/" ++ Data),
    {noreply, State};
handle_info({_Pid, {data, {_Flag, "NEW_REDUCE_RESULT " ++ Data}}},
            State = {JobId, TaskId, Time, TaskType, Progname, _StartedPids}) ->
    chronicler:debug("~w : REDUCE: New finalize task: ~ts~n", [?MODULE, Data]),
    taskFetcher:new_task({JobId, TaskId, Time, TaskType, Progname},
                         finalize, "/results/"),
    {noreply, State};
handle_info({_Pid, {data, {_Flag, "ERROR " ++ Data}}}, State) ->
    chronicler:error("~w : ERROR: ~ts~n", [?MODULE, Data]),
    {noreply, State};
handle_info({_Pid, {data, {_Flag, "LOG " ++ Data}}}, State) ->
    chronicler:user_info("~w : LOG: ~ts~n", [?MODULE, Data]),
    {noreply, State};
handle_info({Pid, {data, {_Flag, "Segmentation fault"}}},
            State = {JobId, TaskId, Time, TaskType, Progname, _StartedPids}) ->
    chronicler:error("~w : Process ~p exited with reason: Segmentation fault",
		     [?MODULE, Pid]),
    taskFetcher:error({self(), error,
                       {JobId, TaskId, Time, TaskType, Progname}}),
    {stop, normal, State};
handle_info({_Pid, {data, {_Flag, Data}}}, State) ->
    chronicler:info(io_lib:format("~w : PORT PRINTOUT: ~ts~n",[?MODULE, Data])),
    {noreply, State};
handle_info({Pid, {exit_status, Status}}, State =
            {JobId, TaskId, Time, TaskType, Progname, _StartedPids})
  when Status == 0 ->
    chronicler:debug("~w : Process ~p exited normally~n", [?MODULE, Pid]),
    taskFetcher:task_done({self(), done,
                               {JobId, TaskId, Time, TaskType, Progname}}),
    {stop, normal, State};
handle_info({Pid, {exit_status, Status}},
            State = {JobId, TaskId, Time, TaskType, Progname, _StartedPids}) ->
    chronicler:error("~w : Process ~p exited with status: ~p~n",
		     [?MODULE, Pid, Status]),
    taskFetcher:error({self(), error,
                               {JobId, TaskId, Time, TaskType, Progname}}),
    {stop, normal, State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    chronicler:warning("~w : Received unexpected handle_info call.~n"
                       "Info: ~p~n",
                       [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% Logs and discards unexpected messages.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) -> 
    chronicler:debug("~w : Received terminate call.~n"
                     "Reason: ~p~n",
                     [?MODULE, Reason]),
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
    chronicler:debug("~w : Received code_change call.~n"
                     "Old version: ~p~n"
                     "Extra: ~p~n",
                     [?MODULE, OldVsn, Extra]),
    {ok, State}.
