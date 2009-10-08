# A very basic Makefile (this line is a comment)
APPNAME = slave
ERLC_FLAGS= -I include +debug_info
C_FLAGS= -I/usr/local/lib/erlang/usr/include -I/usr/lib/erlang/usr/include/
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS= 

all: $(OBJECTS) test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

test: $(OBJECTS)
	mkdir tests
	mv ebin/*_tests.beam tests/
	erl -noshell -pa ebin -pa tests -eval 'eunit:test("tests",[verbose])' -s init stop
	mv tests/*_tests.beam ebin/
	rm -rf tests/

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
