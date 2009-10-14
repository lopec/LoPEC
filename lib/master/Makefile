# A very basic Makefile (this line is a comment)
APPNAME = master
ERLC_FLAGS= -I include +debug_info
SOURCES= $(wildcard src/*.erl)
TESTFILES= $(wildcard test/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
TESTOBJECTS= $(TESTFILES:test/%.erl=test/%.beam)

build: $(OBJECTS)

all: $(OBJECTS) permissions test docs

permissions:
	chmod 400 ~/.erlang.cookie

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o test/ $<

clean:
	-rm -f ebin/*.beam test/*.beam

test: $(OBJECTS) $(TESTOBJECTS)
	erl -sname test -noshell -pa ../common/ebin -pa ../ecg/ebin -pa ../logger/ebin -pa ebin -pa test -eval 'eunit:test("test",[verbose])' -s init stop

logger:
	erlc $(ERLC_FLAGS) src/logger/*.erl -o $(PWD)/ebin/
	-mv *.beam ebin/    

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
