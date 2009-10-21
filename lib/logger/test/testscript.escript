#!/usr/bin/env escript
%%! +W w -pa ebin -pa test

main(_Args) ->
	cover:compile_directory("src"),
	eunit:test("test",[]),
    cover:analyse_to_file(chronicler, "test/chronicler_coverage.html", [html]),
    {ok, Ans} = cover:analyse(chronicler, coverage),
    [io:format("Test coverage of ~p:~p/~B : ~B%~n", [Module, Fun, Arity, trunc(100 * Covered/(Covered + NotCovered))]) ||
        {{Module, Fun, Arity}, {Covered, NotCovered}} <- Ans] .

%	erl -noshell +W w -pa ebin -pa test -eval "cover:compile_directory(\"src\"), \
%	    eunit:test(\"test\",[]), cover:analyse_to_file(chronicler, \
%	    \"test/chronicler_coverage.html\", [html]), io:format(\"{covered, not covered} ~p\", \
%	    [cover:analyse(chronicler, coverage)])." -s init stop
