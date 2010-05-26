.PHONY: all clean run_test

MAKEFLAGS=sw

all:
	$(MAKE) -C apps/ all

clean:
	$(MAKE) -C apps/ clean

run_test:
	$(MAKE) -C apps/ run_test

dialyzer:
	$(MAKE) -C apps/ dialyzer

dialyzer_ignoreerrors:
	$(MAKE) -C apps/ dialyzer_ignoreerrors
