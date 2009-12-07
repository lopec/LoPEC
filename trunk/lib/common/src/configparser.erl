%% Made by Burbas
-module(configparser).
-export([read_config/2, parse/2]).

%%--------------------------------------------------------------------
%% @doc
%% Go throu the List and looks if there exist a Key. If so it returns
%% the value of that key.
%%
%% @spec parse(Key, List) -> 
%%                                  {ok, Value} |
%%                                  {error, not_found}
%% @end
%%--------------------------------------------------------------------
parse(Key, []) ->
    chronicler:debug("~w : Could not find the key '~w'~n", [?MODULE, Key]),
    {error, not_found};
parse(Key, [{Key, Value} | _Config]) ->
    {ok, Value};
parse(Key, [_Other | Config]) ->
    parse(Key, Config).

read_config(File, Key) ->
    {Ret, Config} = file:consult(File),
    case Ret of 
        error -> chronicler:debug("~w : Could not find the configfile '~w'~n",
				    [?MODULE, File]),
                {error, Config};
        ok -> Value = parse(Key, Config),
            Value
    end.
