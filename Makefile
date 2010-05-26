.PHONY: all clean run_test

MAKEFLAGS=sw

all:
	$(MAKE) -C apps/ all
	$(MAKE) -C nodes/ all

clean:
	$(MAKE) -C apps/ clean
	$(MAKE) -C nodes/ clean

run_test:
	$(MAKE) -C apps/ run_test
	$(MAKE) -C utils/ run_test

dialyzer:
	$(MAKE) -C apps/ dialyzer

dialyzer_ignoreerrors:
	$(MAKE) -C apps/ dialyzer_ignoreerrors
