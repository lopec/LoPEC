build:
	$(MAKE) -C common build
	$(MAKE) -C logger build
	$(MAKE) -C ecg build
	$(MAKE) -C master build
	$(MAKE) -C slave build

all:
	$(MAKE) -C common all
	$(MAKE) -C logger all
	$(MAKE) -C ecg all
	$(MAKE) -C master all
	$(MAKE) -C slave all

test:
	$(MAKE) -C common test
	$(MAKE) -C logger test
	$(MAKE) -C ecg test
	$(MAKE) -C master test
	$(MAKE) -C slave test

clean:
	$(MAKE) -C common clean
	$(MAKE) -C logger clean
	$(MAKE) -C ecg clean
	$(MAKE) -C master clean
	$(MAKE) -C slave clean

docs:
	$(MAKE) -C master docs
	$(MAKE) -C slave docs
	$(MAKE) -C logger docs
	$(MAKE) -C ecg docs
	$(MAKE) -C common docs
