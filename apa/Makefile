# A very basic Makefile (this line is a comment)
APPNAME = master
ERLC_FLAGS= -I include +debug_info
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)

all: $(OBJECTS) permissions test docs

permissions:
	chmod 400 ~/.erlang.cookie

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

test:
	erl -sname test -noshell -pa ebin -eval 'eunit:test("ebin",[verbose])' -s init stop

logger:
	erlc $(ERLC_FLAGS) src/logger/*.erl -o $(PWD)/ebin/
	-mv *.beam ebin/    

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
