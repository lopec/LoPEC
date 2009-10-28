all:
	$(MAKE) -C common all
	$(MAKE) -C chronicler all
	$(MAKE) -C ecg all
	$(MAKE) -C master all
	$(MAKE) -C slave all

build:
	$(MAKE) -C common build
	$(MAKE) -C chronicler build
	$(MAKE) -C ecg build
	$(MAKE) -C master build
	$(MAKE) -C slave build

test:
	$(MAKE) -C common test
	$(MAKE) -C chronicler test
	$(MAKE) -C ecg test
	$(MAKE) -C master test
	$(MAKE) -C slave test

clean:
	$(MAKE) -C common clean
	$(MAKE) -C chronicler clean
	$(MAKE) -C ecg clean
	$(MAKE) -C master clean
	$(MAKE) -C slave clean

docs:
	$(MAKE) -C master docs
	$(MAKE) -C slave docs
	$(MAKE) -C chronicler docs
	$(MAKE) -C ecg docs
	$(MAKE) -C common docs
