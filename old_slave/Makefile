# A very basic Makefile (this line is a comment)
APPNAME = slave
ERLC_FLAGS= -I include +debug_info
C_FLAGS= 
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS= 

all: $(OBJECTS) port test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

test: $(OBJECTS)
	mv ebin/*_tests.beam tests/
	erl -noshell -pa ebin -pa tests -eval 'eunit:test("tests",[verbose])' -s init stop
	mv tests/*_tests.beam ebin/
	rm -rf tests/

port:
	mkdir tests
	gcc -o tests/port_test src/port/port.c

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
