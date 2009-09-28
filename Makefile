# A very basic Makefile (this line is a comment)
ERLC_FLAGS= -I include +debug_info 
SOURCES= src/foo.erl src/bar.erl 
HEADERS= include/foo.hrl 
OBJECTS= $(SOURCES:src/%.erl=ebin/%.beam)

all: 
	$(OBJECTS) 

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS)
