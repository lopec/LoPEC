# A very basic Makefile (this line is a comment)
APPNAME = ecg
DEBUG_FLAGS= -Ddebug
ERLC_FLAGS= -I include +debug_info
C_FLAGS= -I/usr/local/lib/erlang/usr/include -I/usr/lib/erlang/usr/include/
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
TESTFILES= $(wildcard test/*_tests.erl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
TESTOBJECTS= $(TESTFILES:test/%.erl=test/%.beam)
DOC_OPTS= 

all: build test docs

build: $(OBJECTS)

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) $(DEBUG_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) $(DEBUG_FLAGS) -o test/ $<

clean:
	-rm -f ebin/*.beam test/*.beam test/*.html

.PHONY: test
test: $(TESTOBJECTS)
	../../testscript.escript $(SOURCES)

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" \
	    -s init stop
