# A very basic Makefile (this line is a comment)
APPNAME = slave
ERLC_FLAGS= -I include +debug_info
SOURCES= $(wildcard src/*.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS= {private, true}

all: $(OBJECTS) test docs

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)

test: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test("ebin",[verbose])' -s init stop

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
