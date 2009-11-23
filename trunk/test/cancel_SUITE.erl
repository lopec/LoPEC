-module(cancel_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [cancel_resume_test].

init_per_suite(Config) ->
    {ok, Hostname} = inet:gethostname(),

    % Args argument dosent seem to work :(
    {ok, Master} = slave:start(Hostname, master),
        %, "+W w -pa ../,,/test -pa ../../lib/*/ebin"),
    {ok, Slave} = slave:start(Hostname, slave),
        %, "+W w -pa ../,,/test -pa ../../lib/*/ebin"),

    spawn(Master, code, add_path, ["../../test/"]),
    spawn(Master, code, load_file, [cancel_SUITE]),

    spawn(Slave, code, add_path, ["../../test/"]),
    spawn(Slave, code, load_file, [cancel_SUITE]),

    [{master, Master}, {slave, Slave} | Config].

end_per_suite(Config) ->
    {master, Master} = lists:keyfind(master, 1, Config),
    {slave, Slave} = lists:keyfind(slave, 1, Config),

    ok = slave:stop(Master),
    ok = slave:stop(Slave),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

cancel_resume_test(Config) ->
    {master, Master} = lists:keyfind(master, 1, Config),
    {slave, Slave} = lists:keyfind(slave, 1, Config),

    MasterControll = fun(F) ->
            receive
                {request, {returnPid, From}} ->
                    From ! {reply, self()},
                    F(F);
                {request, {start, From}} ->
                    code:add_path("../../lib/common/ebin"),
                    ok = application:start(common),
                    code:add_path("../../lib/chronicler/ebin"),
                    ok = application:start(chronicler),
                    code:add_path("../../lib/ecg/ebin"),
                    ok = application:start(ecg),
                    code:add_path("../../lib/master/ebin"),
                    ok = application:start(master),

                    %% We hate database
                    %%db:create_tables(ram_copies),

                    chronicler:set_logging_level([user_info, error]),
                    ok = chronicler:info(masterStarted),
                    From ! {reply, ok},
                    F(F);
                {request, {stop, From}} ->
                    ok = application:stop(master),
                    ok = application:stop(ecg),
                    ok = application:stop(chronicler),
                    ok = application:stop(common),

                    From ! {reply, ok},
                    F(F);
                {request, {add_jobs, From}} ->
                    {ok, Pid1} = listener:add_job(wordcount, mapreduce,
                        test, 1, "/storage/test/lol.txt"),
                    {ok, Pid2} = listener:add_job(wordcount, mapreduce,
                        test, 1, "/storage/test/lol.txt"),
                    chronicler:user_info("Job1: ~p~n"
                                         "Job2: ~p", [Pid1, Pid2]),
                    From ! {reply, {Pid1, Pid2}},
                    F(F);
                {request, {cancel_job, Job, From}} ->
                    ok = listener:cancel_job(Job),
                    From ! {reply, ok},
                    F(F);
                {request, {check_status, Job, From}} ->
                    {error, _REASON} =  db:get_job(Job),
                    From ! {reply, ok},
                    F(F)
            end
    end,

    SlaveControll = fun(F) ->
            receive
                {request, {returnPid, From}} ->
                    From ! {reply, self()},
                    F(F);
                {request, {start, From}} ->
                    code:add_path("../../lib/common/ebin"),
                    ok = application:start(common),
                    code:add_path("../../lib/chronicler/ebin"),
                    ok = application:start(chronicler),
                    chronicler:set_logging_level([]),
                    code:add_path("../../lib/slave/ebin"),
                    ok = application:start(slave),

                    From ! {reply, ok},
                    F(F)
            end
    end,

    M = spawn(Master, fun() -> MasterControll(MasterControll) end),
    S = spawn(Slave, fun() -> SlaveControll(SlaveControll) end),

    M ! {request, {returnPid, self()}},
    receive
        {reply, M} ->
            ok
    end,

    S ! {request, {returnPid, self()}},
    receive
        {reply, S} ->
            ok
    end,

    M ! {request, {start, self()}},
    receive
        {reply, ok} ->
            ok
    end,

    S ! {request, {start, self()}},
    receive
        {reply, ok} ->
            ok
    end,

    M ! {request, {add_jobs, self()}},
    receive
        {reply, {JobId1, JobId2}} ->
                {JobId1, JobId2}
    end,

    %Job1 shold now have started, lets pause it.
    M ! {request, {cancel_job, JobId1, self()}},
    receive
        {reply, ok} ->
                ok
    end,

    timer:sleep(1000),

    %Job2 shold now be done, lets see if Job1 is alive :O
    M ! {request, {check_status, JobId1, self()}},
    receive
        {reply, ok} ->
                ok
    end,

    timer:sleep(1000),

    M ! {request, {stop, self()}},
    receive
        {reply, ok} ->
                ok
    end,

    ok.
