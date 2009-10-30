#!/usr/bin/env escript
%%! +W w -pa ../ecg/ebin ../common/ebin ../chronicler/ebin ../ecg/ebin ../master/ebin ../slave/ebin -pa test
%% A bit ugly above, dunno what we can do about it...

main(Args) ->
    %Turn off terminal output
    error_logger:tty(false),

    %Compile and run tests
    cover:compile_directory("src"),
    eunit:test("test",[verbose]),

    %Find the modules to analyse
    Files = [lists:last(string:tokens(X, "/")) || X <- Args],
    Modules = [list_to_atom(string:sub_string(X, 1, length(X) - 4))
               || X <- Files],

    TotalCovered = 0, TotalLines = 0,
    io:format("~n"),
    analyse_all(Modules, {TotalCovered, TotalLines}).

%% The below could be refactored to a fold, but why bother?
analyse_all([], {TotalCovered, TotalLines}) ->
    io:format("~n=======================================================~n"),
    io:format("  Total coverage of whole application ~B%~n",
              [trunc(100 * TotalCovered / TotalLines)]);
analyse_all([M | Tail], {CoveredAck, LinesAck}) ->
    cover:analyse_to_file(M, lists:concat(["test/", M, "_coverage.html"]),
                          [html]),
    {ok, Ans} = cover:analyse(M, coverage),

    CoverageResult =
        [[Fun, Arity, trunc(100 * Covered/(Covered + NotCovered))]
         || {{_, Fun, Arity}, {Covered, NotCovered}} <- Ans, NotCovered /= 0],

    Statistics =
        [{Covered, Covered + NotCovered} || {_, {Covered, NotCovered}} <- Ans],

    {TotalCovered, TotalLines} =
        lists:foldr(fun({Cov, Lin}, {CovSum, LinSum}) ->
                            {Cov + CovSum, Lin + LinSum}
                    end,
                    {CoveredAck, LinesAck} , Statistics),

    io:format("Test coverage of ~p:~n", [M]),
    [io:format("  ~p/~B : ~B%~n", X) || X <- CoverageResult],
    analyse_all(Tail, {TotalCovered, TotalLines}).
