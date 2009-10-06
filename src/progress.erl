%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson <>
%%% @doc
%%% Provides get_progress and get_progress_full which take a JobId
%%% as argument and returns a String with information about the
%%% states of the tasks of that job
%%%
%%% TODO:
%%% 1. Function for quering the list of updated tasks at regular
%%% intervals and calculate progress from the number of total tasks
%%% to the number of tasks of various states.
%%% 2. Create a set of unit tests for the progress functions; make
%%% sure the calls to the database works as intended.
%%% @end
%%% Created : 05 Okt 2009 by Gustav Simonsson <>
%%%-------------------------------------------------------------------
-module(progress).
-behaviour(gen_server).

%% API
-export([start_link/0, get_progress/1, get_progress_full/1, get_tasks_of_state/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the progress process
%%
%% @spec start_link() -> {ok, Pid} |
%%       ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, progress},progress,[], []).

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
init([]) ->
%    if Interval > 0 ->
%	    timer:send_interval(Interval,[]),
%	    ok;
%       true -> ok
%    end.
    {ok}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_info(Interval, JobId) ->
%%                                   {to be determined}
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a string containing information about the number of jobs
%% done and a percentage approximation.
%% 
%% @spec get_progress(JobId) -> String
%% @end
%%--------------------------------------------------------------------
get_progress(JobId) ->
    Tasks = db:list_tasks(JobId),
    TasksTotal = length(Tasks),
    TasksDone = get_tasks_of_state(Tasks,done),
    lists:flatten(io_lib:format("~p/~w tasks done",
		 [TasksDone, TasksTotal])) ++ " (" ++
	         percentage_string(TasksDone, TasksTotal) ++ ")".

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Returns a string containing information about the number of jobs
%% done, available, assigned and reserved  and a percentage
%% approximation of these states.
%% 
%% @spec get_progress_full(JobId) -> String
%% @end
%%--------------------------------------------------------------------
get_progress_full(JobId) ->
    Tasks = db:list_tasks(JobId),
    TasksTotal = length(Tasks),
    
    Done_Progress = get_progress(JobId),

    TasksAvailable = get_tasks_of_state(Tasks, available),
    Available_Progress = lists:flatten(io_lib:format(
		 " ~p/~w available", [TasksAvailable, TasksTotal])) ++
	         " (" ++
	         percentage_string(TasksAvailable, TasksTotal) ++ ")",

    TasksReserved = get_tasks_of_state(Tasks, reserved),
    Reserved_Progress = lists:flatten(io_lib:format(
		 " ~p/~w reserved", [TasksReserved, TasksTotal])) ++
	         " (" ++
	         percentage_string(TasksReserved, TasksTotal) ++ ")",

    TasksAssigned = get_tasks_of_state(Tasks, assigned),
    Assigned_Progress = lists:flatten(io_lib:format(
		 " ~p/~w assigned", [TasksAssigned, TasksTotal])) ++
	         " (" ++
	         percentage_string(TasksAssigned, TasksTotal) ++ ")",
    
    Done_Progress ++ Available_Progress ++ Reserved_Progress ++ Assigned_Progress.

percentage_string(SubTasks, TasksTotal) ->
    lists:sublist(lists:flatten(io_lib:format(
        "~p", [(SubTasks / TasksTotal) * 100])), 4) ++ "%".


get_tasks_of_state([], _) ->
    0;
get_tasks_of_state([H|T], State) ->
    {task_state, _} = db:get_task_state(H),
    if task_state == State ->
	    1 + get_tasks_of_state(T, State);
       true ->
	    0 + get_tasks_of_state(T, State)
    end.
   

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
terminate(_Reason, _State) -> %not used
    ok.

code_change(_OldVsn, State, _Extra) -> %not used
    {ok, State}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% Receives data from task listener and adds it to task list
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> %not used
    {noreply, State}.
