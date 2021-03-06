all: build common_test dialyze docs
#	$(MAKE) -C lib all
#	$(MAKE) dialyze

build:
	$(MAKE) -C lib build

test:
	$(MAKE) -C lib test

clean:
	$(MAKE) -C lib clean

docs:
	$(MAKE) -C lib docs

master:
	$(MAKE) -C lib/master build
	$(MAKE) -C lib/ecg build
	$(MAKE) -C lib/common build
	$(MAKE) -C lib/chronicler build

slave:
	$(MAKE) -C lib/slave build
	$(MAKE) -C lib/chronicler build
	$(MAKE) -C lib/common build

master_script: master
	erl -pa lib/master/ebin -pa lib/ecg/ebin -pa lib/chronicler/ebin \
	    -pa lib/common/ebin \
	    -eval "systools:make_script(\"releases/master/start_master\", [local])" \
	    -s init stop

slave_script: slave
	erl -pa lib/slave/ebin -pa lib/chronicler/ebin -pa lib/common/ebin \
	    -eval "systools:make_script(\"releases/slave/start_slave\", [local])" \
	    -s init stop

master_tar: master_script
	erl -pa lib/master/ebin -pa lib/ecg/ebin -pa lib/chronicler/ebin \
	    -pa lib/common/ebin \
	    -eval "systools:make_tar(\"releases/master/start_master\")" \
	    -s init stop

slave_tar: slave_script
	erl -pa lib/slave/ebin -pa lib/chronicler/ebin -pa lib/common/ebin \
	    -eval "systools:make_tar(\"releases/slave/start_slave\")" \
	    -s init stop

testing_script: master slave
	erl -pa lib/master/ebin -pa lib/ecg/ebin -pa lib/chronicler/ebin \
	    -pa lib/common/ebin -pa lib/slave/ebin \
	    -eval "systools:make_script(\"releases/testing/start_testing\", [local])" \
	    -s init stop

test.spec: test.spec.in
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

common_test: test.spec build
	mkdir -p log
	run_test +W w -pa $(PWD)/lib/*/ebin -spec test.spec -cover coverspec

dialyze: build
	@echo "\n -- Running dialyzer --\n"
	dialyzer --verbose -c lib/*/ebin | sed 's/^\(\w*\.erl\)/\n\1/'