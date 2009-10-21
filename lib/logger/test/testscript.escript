#!/usr/bin/env escript
%%! +W w -pa ebin -pa test

main(Args) ->
    %Compile and run tests
	cover:compile_directory("src"),
	eunit:test("test",[]),

    %Find the modules to analyse
    Files = [lists:last(string:tokens(X, "/")) || X <- Args],
    Modules = [list_to_atom(string:sub_string(X, 1, length(X) - 4)) || X <- Files],

    analyse_all(Modules).

analyse_all([]) -> ok;
analyse_all([M | Tail]) ->
    cover:analyse_to_file(M, "test/" ++ atom_to_list(M) ++ "_coverage.html", [html]),
    {ok, Ans} = cover:analyse(M, coverage),

    CoverageResult =
        [[Module, Fun, Arity, trunc(100 * Covered/(Covered + NotCovered))] || {{Module, Fun, Arity}, {Covered, NotCovered}} <- Ans, NotCovered /= 0],
    [io:format("Test coverage of ~p:~p/~B : ~B%~n", X) || X <- CoverageResult],
    analyse_all(Tail).

%    cover:analyse_to_file(chronicler, "test/chronicler_coverage.html", [html]),
%    {ok, Ans} = cover:analyse(chronicler, coverage),
%    [io:format("Test coverage of ~p:~p/~B : ~B%~n", [Module, Fun, Arity, trunc(100 * Covered/(Covered + NotCovered))]) ||
%        {{Module, Fun, Arity}, {Covered, NotCovered}} <- Ans].

%	erl -noshell +W w -pa ebin -pa test -eval "cover:compile_directory(\"src\"), \
%	    eunit:test(\"test\",[]), cover:analyse_to_file(chronicler, \
%	    \"test/chronicler_coverage.html\", [html]), io:format(\"{covered, not covered} ~p\", \
%	    [cover:analyse(chronicler, coverage)])." -s init stop

%Arguments: ["src/chronicler.erl","src/chronicler_app.erl", "src/chronicler_sup.erl","src/externalLogger.erl", "src/fileLogger.erl"]

