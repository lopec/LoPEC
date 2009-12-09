-module(io_module_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [fs_io_test].

init_per_suite(Config) ->
    error_logger:tty(false),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(fs_io_test, Config) ->
    io_module:start_link(fs_io_module, []),
    Config;
init_per_testcase(riak_io_test, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    io_module:stop(),
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

fs_io_test(_Config) ->
    InputData = <<"Ni hao">>,
    ok = io_module:put(123465789, 9876543210, InputData),
    {ok, InputData} = io_module:get(123465789, 9876543210),
    ok.

riak_io_test(_Config) ->
    InputData = <<"Ni hao">>,
    ok = io_module:put(123465789, 9876543210, InputData),
    {ok, InputData} = io_module:get(123465789, 9876543210),
    ok.
