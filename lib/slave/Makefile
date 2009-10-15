# A very basic Makefile (this line is a comment)
APPNAME = slave
ERLC_FLAGS= -I include +debug_info
C_FLAGS= 
SOURCES= $(wildcard src/*.erl)
TESTFILES= $(wildcard test/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
TESTOBJECTS= $(TESTFILES:test/%.erl=test/%.beam)
DOC_OPTS= 

build: $(OBJECTS) $(TESTOBJECTS)

all: $(OBJECTS) $(TESTOBJECTS) port test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o test/ $<

clean:
	rm -f ebin/*.beam test/*.beam

.PHONY: test
test: $(OBJECTS) $(TESTOBJECTS)
	erl -noshell -pa ebin -pa test -eval 'eunit:test("test",[verbose])' -s init stop

port:
	mkdir tests
	gcc -o tests/port_test examples/port_example.c
	rm -rf tests

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
