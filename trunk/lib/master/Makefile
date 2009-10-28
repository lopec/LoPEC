# A very basic Makefile (this line is a comment)
APPNAME = master
DEBUG_FLAGS= 
ERLC_FLAGS= -I include +debug_info
SOURCES= $(wildcard src/*.erl)
TESTFILES= $(wildcard test/*_tests.erl)
HEADERS= $(wildcard include/*.hrl)
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)
TESTOBJECTS= $(TESTFILES:test/%.erl=test/%.beam)

all: build test docs

build: $(OBJECTS)

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) $(DEBUG_FLAGS) -o ebin/ $<

test/%.beam : test/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) $(DEBUG_FLAGS) -o test/ $<

clean:
	-rm -f ebin/*.beam test/*.beam test/*.html

.PHONY: test
test: $(TESTOBJECTS) chronicler
	../../testscript.escript $(SOURCES)

chronicler:
	erlc $(ERLC_FLAGS) ../chronicler/src/*.erl -o $(PWD)/ebin/
	-mv *.beam ebin/    

docs: 
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
