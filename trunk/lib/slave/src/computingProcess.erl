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
-export([start_link/5,
         stop/0,
         stop_job/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {job_id,
                task_id,
                started,
                task_type,
                prog_name,
                pids_to_kill = [],
                storage_keys,
                port}).

kill_all_procs([]) ->
    chronicler:info("~w : No moar PIDs to keel.", [?MODULE]),
    [];
kill_all_procs([{pid, Pid}|T]) ->
    chronicler:info("~w : Killing PID = ~p~n", [?MODULE, Pid]),
    os:cmd("kill -9 " ++ binary_to_list(Pid)),
    kill_all_procs(T);
kill_all_procs([H|T]) ->
    chronicler:info("~w : Killing PID (thru pid-file) = ~p~n", [?MODULE, H]),
    os:cmd("kill -9 `cat " ++ H ++ "`"),
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
%% @spec start_link(ProgName, TaskType, JobId, StorageKeys, TaskId) ->
%%           {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ProgName, TaskType, JobId, StorageKeys, TaskId) ->
    chronicler:info("~w : application starting~n", [?MODULE]),
    gen_server:start_link({local, ?SERVER},?MODULE,
                          [ProgName, TaskType, JobId, TaskId, StorageKeys], []).

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
    gen_server:call(?MODULE, stop_job),
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
init([ProgName, TaskType, JobId, TaskId, StorageKeys]) ->
    {ok, Root} = configparser:read_config(?CONFIGFILE, cluster_root),
    {ok, Platform} = configparser:read_config(?CONFIGFILE, platform),
    ProgRoot = lists:concat([Root, "/programs/", ProgName, "/"]),
    ProgPath = lists:concat([ProgRoot, "/script.", Platform]),
    JobRoot = lists:concat([Root, "/tmp/", JobId, "/"]),
    PidPath = lists:concat([JobRoot, "/pid/", node(), "/"]),
    lists:foreach(fun (Path) -> filelib:ensure_dir(Path),
                                file:change_mode(Path, 8#777) end,
                  [PidPath]),
    NodeCount = integer_to_list(length(nodes())),
    {Bucket, _Keys} = StorageKeys,
    TaskKey = lists:last(string:tokens(binary_to_list(Bucket), "/")),
    ArgList = [atom_to_list(TaskType), TaskKey, PidPath]
    % Add the number of nodes to the arg list if the task is a split
              ++ case TaskType of
                     split -> [NodeCount];
                     _ -> [] end,
    PortArgs = [binary, {packet, 4}, use_stdio, stderr_to_stdout,
                exit_status, {cd, ProgRoot}, {args, ArgList}],
    chronicler:debug("~w : starting ~s", [?MODULE, ProgPath]),
    Port = open_port({spawn_executable, ProgPath}, PortArgs),
    {ok, #state{job_id = JobId, task_id = TaskId, started = now(),
                task_type = TaskType, prog_name = ProgName,
                port = Port, storage_keys = StorageKeys}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(stop_job, From, State) ->  {reply, job_stopped, State}
%% @end
%%--------------------------------------------------------------------
%% HERE IS THE EVUL PRÅBLEM
handle_call(stop_job, _From,
            State = #state{job_id = JobId, pids_to_kill = Pids}) ->
    {ok, Root} = configparser:read_config("/etc/clusterbusters.conf",
                                          cluster_root),
    chronicler:info("~w: About to kill ~p", [?MODULE, Pids]),
    PidPath = lists:concat([Root, "/tmp/", JobId, "/pid/", node(), "/*.pid"]),
    PidFiles = filelib:wildcard(PidPath),
    chronicler:info("~w: About to kill ~p", [?MODULE, PidFiles]),
    kill_all_procs(Pids),
    kill_all_procs(PidFiles),

    {reply, job_stopped, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:debug("~w : Received unexpected handle_call call.~n"
                       "Message: ~p~n"
                       "From: ~p~n",
                       [?MODULE, Msg, From]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {stop, Reason, State}
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
    chronicler:debug("~w : Received unexpected handle_cast call.~n"
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
handle_info({_Pid, {data, <<"NEW_PID ", Pid/binary>>}}, State) ->
    chronicler:debug("~w : new pid ~p", [?MODULE, Pid]),
    NewPids = [Pid | State#state.pids_to_kill],
    {noreply, State#state{pids_to_kill = NewPids}};
handle_info({_Pid, {data, <<"GET_DATA">>}},State) ->
    chronicler:debug("~w : got data request", [?MODULE]),
    NewState = get_next_input(State),
    {noreply, NewState};
handle_info({_Pid, {data, <<"NEW_MAP ", KeysAndData/binary>>}},State) ->
    chronicler:debug("~w : got map", [?MODULE]),
    add_task(map, KeysAndData, State),
    {noreply, State};
handle_info({_Pid, {data, <<"NEW_REDUCE ", KeysAndData/binary>>}}, State) ->
    chronicler:debug("~w : got reduce", [?MODULE]),
    add_task(reduce, KeysAndData, State),
    {noreply, State};
handle_info({_Pid, {data, <<"NEW_RESULT ", KeysAndData/binary>>}}, State) ->
    chronicler:debug("~w : got result", [?MODULE]),
    add_finalize(KeysAndData, State),
    {noreply, State};
handle_info({_Pid,
             {data, <<"NEW_FINAL_RESULT ", KeyAndData/binary>>}}, State) ->
    chronicler:debug("~w : got final result", [?MODULE]),
    add_final_result(KeyAndData, State),
    {noreply, State};
handle_info({_Pid, {data, <<"NO_FINALIZING_PLZ">>}}, State) ->
    chronicler:debug("~w : no finalizing requested", [?MODULE]),
    no_finalize(State),
    {noreply, State};
handle_info({_Pid, {data, <<"ERROR ", Message/binary>>}}, State) ->
    chronicler:error(fix_me, "~w : ERROR: ~ts~n", [?MODULE, Message]),
    {noreply, State};
handle_info({_Pid, {data, <<"LOG ", Message/binary>>}}, State) ->
    chronicler:user_info(fix_me, "~w : LOG: ~ts~n", [?MODULE, Message]),
    {noreply, State};
handle_info({Pid, {data, <<Message/binary>>}}, State) ->
    chronicler:error(fix_me, "~w : Task failed, unrecognized output: ~ts~nfrom ~w",
                     [?MODULE, Message, Pid]),
    fail_task(State),
    {stop, normal, State};
handle_info({Pid, {exit_status, Status}},
            State = #state{job_id = JobId, task_id = TaskId, started = Time,
                           task_type = TaskType, prog_name = ProgName})
  when Status == 0 ->
    chronicler:debug("~w : Process ~p exited normally~n", [?MODULE, Pid]),
    taskFetcher:task_done({self(), done,
                           {JobId, TaskId, Time, TaskType, ProgName}}),
    {stop, normal, State};
handle_info({Pid, {exit_status, Status}}, State) ->
    chronicler:error(fix_me, "~w : Process ~p exited with status: ~p~n",
                     [?MODULE, Pid, Status]),
    fail_task(State),
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
    chronicler:debug("~w : Received unexpected handle_info call.~n"
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

fail_task(#state{job_id = JobId, task_id = TaskId, started = Time,
                 task_type = TaskType, prog_name = ProgName}) ->
    taskFetcher:error({self(), error,
                       {JobId, TaskId, Time, TaskType, ProgName}}).

get_next_input(State = #state{port = Port, storage_keys = {Bucket, Keys}}) ->
    case Keys of
        [] ->
            port_command(Port, <<"NONE\n">>),
            State;
        [Key | Moar] ->
            {ok, Data} = io_module:get(Bucket, Key),
            port_command(Port, <<"SOME\n", Key/binary, "\n", Data/binary>>),
            _NewState = State#state{storage_keys = {Bucket, Moar}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the given data under the given key and bucket and creates a
%% new task via taskFetcher.
%%
%% @spec add_task(TaskType, KeysAndData, State) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
add_task(TaskType, KeysAndData, #state{job_id = JobId,prog_name = ProgName}) ->
    {TaskKey, DataKey, Data} = unpack(KeysAndData),
    Bucket =
        list_to_binary(lists:concat([JobId,"/",TaskType,"/",TaskKey,"/"])),
    io_module:put(Bucket, DataKey, Data),
    taskFetcher:new_task(JobId, ProgName, TaskType, Bucket, DataKey).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the given data under the given key and bucket.
%%
%% @spec add_finalize(KeyAndData, State) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
add_finalize(KeyAndData, #state{job_id = JobId, prog_name = ProgName}) ->
    {_TaskKey, DataKey, Data} = unpack(KeyAndData, "", <<>>), %% ZOMGHAX
    Bucket = list_to_binary(lists:concat([JobId,"/finalize/"])),
    io_module:put(Bucket, DataKey, Data),
    taskFetcher:new_task(JobId, ProgName, finalize, Bucket, DataKey).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the given data under the given key and bucket.
%%
%% @spec add_final_result(KeyAndData, State) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
add_final_result(KeyAndData, #state{job_id = JobId}) ->
    {_TaskKey, DataKey, Data} = unpack(KeyAndData, "", <<>>), %% ZOMGHAX
    Bucket = list_to_binary(lists:concat([JobId,"/results/"])),
    io_module:put(Bucket, DataKey, Data),
    dispatcher:add_storage_key(JobId, Bucket, DataKey).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% not implemented
%% @todo implement me!
%%
%% @spec no_finalize(State) -> todo
%% @end
%%--------------------------------------------------------------------
no_finalize(State) ->
    todo.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unpacks the given Keys and Data
%% from path given by binaries "TaskKey DataKey\n", Data
%% to a path defined by {"TaskKey", "DataKey", Data}.
%% 
%%
%% @spec unpack(KeysAndData) -> {TaskKey, DataKey, Data}
%% @end
%%--------------------------------------------------------------------
unpack(KeysAndData) ->
    unpack(KeysAndData, "").
unpack(<<" ", Rest/binary>>, TaskKey) ->
    unpack(Rest, TaskKey, <<>>);
unpack(<<C:8, Rest/binary>>, TaskKey) ->
    unpack(Rest, TaskKey ++ [C]).
unpack(<<"\n", Rest/binary>>, TaskKey, DataKey) ->
    {TaskKey, DataKey, Rest};
unpack(<<C:8, Rest/binary>>, TaskKey, DataKey) ->
    unpack(Rest, TaskKey, <<DataKey/binary, C:8>>).
