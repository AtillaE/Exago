LIBDIRS:=$(shell pwd)

MAKEFLAGS=sw

APP_SRC:=$(shell find -L . -maxdepth 3 \( -name '*.app.src' -o -name '*.app' \) -print)
APPS:=$(sort $(shell echo $(APP_SRC) | xargs -n 1 echo | sed -e 's|^\./||' -e 's|/.*$$||'))

.PHONY: all clean app test run_test dialyzer

all: $(addsuffix .all,$(APPS))

# The function submake takes one argument: the directory in which to
# run make.  If this directory does not contain a Makefile, it uses
# make/apps-defs.mk and make/apps-common.mk instead.
submake = $(MAKE) -C $1 \
	  $(shell test -f $1/Makefile || \
	    echo -f \""$(LIBDIRS)/../make/apps-defs.mk"\" \
		 -f \""$(LIBDIRS)/../make/apps-common.mk"\")

%.all: %
	@$(call submake,$<) all

clean: $(addsuffix .clean,$(APPS))
	rm -f .depend

%.clean: %
	@$(call submake,$<) clean

test: all $(addsuffix .test,$(APPS))

%.test: %
	@$(call submake,$<) test

run_test: test $(addsuffix .run_test,$(APPS))

%.run_test: %
	@$(call submake,$<) run_test

app: $(addsuffix .app,$(APPS))

%.app: %
	@$(call submake,$<) app

UTILSDIR=$(LIBDIRS)/../utils
ERLDEP="$(UTILSDIR)/erldep"
APPDEP="$(UTILSDIR)/appdep"

.DELETE_ON_ERROR:

# The firstword+realpath hack serves to find the first of the
# specified files that exists.
appdeps = $(shell $(ERLDEP) -apps $1/src $(firstword $(realpath $1/src/$1.app.src $1/ebin/$1.app)) .)

define depend_template
@echo $1.all: $(addsuffix .all,$2) >>$@
@echo $1.test: $(addsuffix .all,$2) >>$@

endef

sinclude .depend
.depend: $(APP_SRC)
	@echo APPDEP
	@echo >$@
	$(foreach app,$(APPS),$(call depend_template,$(app),$(call appdeps,$(app))))

DIALYZER_FLAGS?=-Wunderspecs
dialyzer:
	dialyzer $(DIALYZER_FLAGS) --src -r `find -L . -name src -print`

dialyzer_ignoreerrors:
	-dialyzer $(DIALYZER_FLAGS) --src -r `find -L . -name src -print`
