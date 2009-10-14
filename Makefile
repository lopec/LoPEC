all:
	$(MAKE) -C lib all

clean:
	$(MAKE) -C lib clean

docs:
	$(MAKE) -C lib docs

master:
	$(MAKE) -C lib/master all
	$(MAKE) -C lib/ecg all
	$(MAKE) -C lib/logger all

slave:
	$(MAKE) -C lib/slave all
	$(MAKE) -C lib/logger all

master_script: master
	erl -pa lib/master/ebin -pa lib/ecg/ebin -pa lib/logger/ebin \
	    -eval "systools:make_script(\"releases/master/start_master\", [local])" \
	    -s init stop

slave_script: slave
	erl -pa lib/slave/ebin -pa lib/logger/ebin \
	    -eval "systools:make_script(\"releases/slave/start_slave\", [local])" \
	    -s init stop

master_tar: master_script
	erl -pa lib/master/ebin -pa lib/ecg/ebin -pa lib/logger/ebin \
	    -eval "systools:make_tar(\"releases/master/start_master\")" \
	    -s init stop

slave_tar: slave_script
	erl -pa lib/slave/ebin -pa lib/logger/ebin \
	    -eval "systools:make_tar(\"releases/slave/start_slave\")" \
	    -s init stop
