# A very basic Makefile (this line is a comment)
APPNAME = slave
ERLC_FLAGS= -I include +debug_info
C_FLAGS= -I/usr/local/lib/erlang/usr/include -I/usr/lib/erlang/usr/include/
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
SOURCES= $(wildcard test/*.erl)
TESTOBJECTS= $(SOURCES:test/%.erl=test/%.beam)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS= 

all: $(OBJECTS) test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o test/ $<
clean:
	-rm $(OBJECTS) $(TESTOBJECTS)

test: $(TESTOBJECTS)
	erl -noshell -pa ebin -pa tests -eval 'eunit:test("test",[verbose])' -s init stop

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
