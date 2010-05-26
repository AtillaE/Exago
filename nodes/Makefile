include vsn.mk
sinclude config.mk

MAKEFLAGS=sw

NODES:=$(subst /,,$(dir $(wildcard */*.rel.src)))

LIBDIRS?=../../apps

export LIBDIRS

NODEVSN?=NOVSN
DAYOFWEEK=$(shell date +%u)
NIGHTLY_VERSION=nightly_$(DAYOFWEEK)

TARBALLS=$(foreach node, $(NODES), $(node)/$(node)-$(NODEVSN).tar.gz)
RELFILES=$(foreach node, $(NODES), $(node)/$(node)-$(NODEVSN).rel)

NIGHTLY_TARBALLS=$(foreach node, $(NODES), $(node)/$(node)-$(NIGHTLY_VERSION).tar.gz)
NIGHTLY_RELFILES=$(foreach node, $(NODES), $(node)/$(node)-$(NIGHTLY_VERSION).rel)

TARGETS=$(foreach node, $(NODES), $(node).target)
PACKAGE=$(foreach node, $(NODES), $(node).package)

.PHONY: clean all tar target nightly package force

all: target
tar: $(TARBALLS)
target: $(TARGETS)
nightly: $(NIGHTLY_TARBALLS)
package: $(PACKAGE)

%-$(NODEVSN).tar.gz: force
	@$(MAKE) -C $(dir $@) tar NODEVSN=$(NODEVSN)

%-$(NIGHTLY_VERSION).tar.gz: force
	@$(MAKE) -C $(dir $@) tar NODEVSN=$(NIGHTLY_VERSION)

%.target:
	$(MAKE) -C $(shell basename $@ .target) target

%.package:
	@NODEVSN=$(NODEVSN) $(MAKE) -C $(dir $@) tar
	@util/inst_build $(basename $@ .package) $(NODEVSN) false

clean:
	@for node in $(NODES); do $(MAKE) -C $$node clean; done


start_all:
	@for node in $(NODES); do $$node/bin/start; done


stop_all:
	@for node in $(NODES); do $$node/bin/stop; done

force: ;
