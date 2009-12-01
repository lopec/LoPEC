-module(io_module_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [iotest].

init_per_suite(Config) ->
    error_logger:tty(false),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    io_module:start_link(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    io_module:stop(),
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

iotest(_Config) ->
    
    InputData = <<"Ni hao">>,
    ok = io_module:put(123465789, 9876543210, InputData),
    InputData = io_module:get(123465789, 9876543210),
    ok.

