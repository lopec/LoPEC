%%% @private
-module(clientSupervisor_tests).
-include_lib("eunit/include/eunit.hrl").

child_init_test_() ->
   {setup,
     fun () -> ok end,
     fun (_) -> ok end,
     fun (_) ->
             {inorder,
              [
               ?_assertMatch({ok, {_, _}}, clientSupervisor:init(no_args))
              ]}
     end
    }.

child_specs_test_() ->
   {setup,
     fun () -> clientSupervisor:init(no_args) end,
     fun (_) -> ok end,
     fun ({ok, {_, ChildSpecs}}) ->
             {inorder,
              [
               ?_assertMatch(ok, supervisor:check_childspecs(ChildSpecs))
              ]}
     end
    }.
