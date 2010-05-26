
.PHONY: clean

$(BASENAME).rel: $(NODE).rel.src
	true

# Some application makefiles accept both ebin/APP.app and app; some
# insist on one or the other.  We try both...
%.app:
	$(MAKE) --no-print-directory -C $(abspath $(dir $@)/..) ebin/$(notdir $@) || \
	$(MAKE) --no-print-directory -C $(abspath $(dir $@)/..) app

clean:
	true

schema:
	erl -sname $(NODE) -noshell -eval "ok = mnesia:create_schema([node()])." -s init stop

### TARGET SYSTEM

RELEASE_FILES=releases/start_erl.data \
	releases/$(NODEVSN)/start.boot releases/$(NODEVSN)/sys.config

# target: prepare the target system, make sure every application is
# compiled.
target: dirs binfiles libs $(RELEASE_FILES)

# start: start the target system, without recompiling applications
start: binfiles dirs $(RELEASE_FILES)
	bin/start

ERLROOT:=$(shell erl -noinput -noshell -eval 'io:format("~s~n", [code:root_dir()])' -s init stop)
ERTSVSN:=$(shell ls -d $(ERLROOT)/erts-*.*.* | tail -1 | sed -e 's|.*erts-||')

# dirs: create the necessary directories
dirs:
	@echo Creating directories
	@mkdir -p bin lib releases/$(NODEVSN) log pipes

binfiles:
	true

ifneq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
REAL_NODE_DIR="$(NODE_DIR)"
else
REAL_NODE_DIR=$(shell cygpath --mixed --short "$(NODE_DIR)")
endif

# libs: symlink applications into lib/, and compile them.  we don't
# remove symlinks to applications no longer used; use "make clean" for
# that.  On Cygwin we can't symlink the applications, so we copy them
# instead.  That breaks the makefiles in the applications, so we can't
# compile them...
libs: $(BASENAME).rel dirs
	@echo Linking erts-$(ERTSVSN)
	@ln -sf $(ERLROOT)/erts-$(ERTSVSN) .
ifneq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
	@echo Compiling applications
	@set -e; for app in lib/*/; do \
		if test -f $$app/Makefile; then \
			$(MAKE) -C "$$app"; \
		elif test -f $$app/Emakefile; then \
			$(MAKE) -C "$$app" \
				-f $(abspath $(LIBDIRS))/../make/apps-defs.mk \
				-f $(abspath $(LIBDIRS))/../make/apps-common.mk; \
		fi; \
	done
else
	@echo Not compiling applications. Hope you did that before.
endif

releases/$(NODEVSN).rel: $(BASENAME).rel
	cp $< $@

releases/$(NODEVSN).script releases/$(NODEVSN).boot: releases/$(NODEVSN).rel libs
	@echo Creating boot script
	@erl -noshell -eval "ok = systools:make_script(\"releases/$(NODEVSN)\", [{path, [\"lib/*/ebin\"]}])." \
		-s erlang halt

releases/$(NODEVSN)/start.boot: releases/$(NODEVSN).boot
	@echo Copying boot script
	@cp $< $@

releases/start_erl.data:
	echo $(ERTSVSN) $(NODEVSN) > $@

# if you need a special configuration, replace
# releases/$(NODEVSN)sys.config with a regular file.  it will not be
# overwritten.
releases/$(NODEVSN)/sys.config:
	ln -s ../../sys.config $@
