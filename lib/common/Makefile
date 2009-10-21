# A very basic Makefile (this line is a comment)
APPNAME = common
ERLC_FLAGS= -I include +debug_info
C_FLAGS= -I/usr/local/lib/erlang/usr/include -I/usr/lib/erlang/usr/include/
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
TESTFILES= $(wildcard test/*.erl)
TESTOBJECTS= $(TESTFILES:test/%.erl=test/%.beam)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS=

build: $(OBJECTS) $(TESTOBJECTS)

all: $(OBJECTS) $(TESTOBJECTS) test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o test/ $<

clean:
	rm -f ebin/*.beam test/*.beam

test: $(TESTOBJECTS)
	erl -noshell -pa ../ecg/ebin -pa ../logger/ebin -pa ../master/ebin \
	    -pa ebin -pa tests -eval 'eunit:test("test",[verbose]).' \
	    -s init stop

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" \
	    -s init stop
