#!/usr/bin/env escript
%%! +W w -pa ../ecg/ebin ../common/ebin ../logger/ebin ../ecg/ebin ../master/ebin ../slave/ebin -pa test

main(Args) ->
    %Turn off terminal output
    error_logger:tty(false),

    %Compile and run tests
	cover:compile_directory("src"),
	eunit:test("test",[]),

    %Find the modules to analyse
    Files = [lists:last(string:tokens(X, "/")) || X <- Args],
    Modules = [list_to_atom(string:sub_string(X, 1, length(X) - 4)) || X <- Files],

    TotalCovered = 0, TotalLines = 0,
    analyse_all(Modules, {TotalCovered, TotalLines}).

analyse_all([], {TotalCovered, TotalLines}) ->
    io:format("       =========================================~n"),
    io:format("       Total coverage of whole application ~B%~n", [trunc(100 * TotalCovered / TotalLines)]);
analyse_all([M | Tail], {CoveredAck, LinesAck}) ->
    cover:analyse_to_file(M, "test/" ++ atom_to_list(M) ++ "_coverage.html", [html]),
    {ok, Ans} = cover:analyse(M, coverage),

    CoverageResult =
        [[Module, Fun, Arity, trunc(100 * Covered/(Covered + NotCovered))] || {{Module, Fun, Arity}, {Covered, NotCovered}} <- Ans, NotCovered /= 0],

    Statistics =
        [{Covered, Covered + NotCovered} || {{_Module, _Fun, _Arity}, {Covered, NotCovered}} <- Ans],

    {TotalCovered, TotalLines} = lists:foldr(fun({Cov, Lin}, {CovSum, LinSum}) -> {Cov + CovSum, Lin + LinSum} end, {CoveredAck, LinesAck} , Statistics),

    [io:format("Test coverage of ~p:~p/~B : ~B%~n", X) || X <- CoverageResult],
    analyse_all(Tail, {TotalCovered, TotalLines}).
