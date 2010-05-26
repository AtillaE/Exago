APPS_DEFS_MK:=$(lastword $(MAKEFILE_LIST))
UTILSDIR=$(shell dirname "$(APPS_DEFS_MK)")/../utils

# ERLDEP and ERLTEST are quoted, as the path might contain
# spaces.
ERLDEP="$(UTILSDIR)/erldep"

ifneq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
ERLTEST="$(UTILSDIR)/erltest"
else
# escript expects non-Cygwin path
ERLTEST=escript "$(shell cygpath --mixed "$(UTILSDIR)/erltest")"
endif

# Extra flags
ERLC_FLAGS?=+debug_info
TEST_ERLC_FLAGS?=
ERLTEST_FLAGS?=

# Find sources, modules, includes, docs etc.
SOURCES=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))
MODULES=$(patsubst src/%.erl,%,$(SOURCES))
INCLUDES=$(wildcard include/*.hrl)
DOCS=doc/edoc-info

TEST_SOURCES=$(wildcard test/*.erl)
TEST_BEAMS=$(patsubst test/%.erl, test/%.beam, $(TEST_SOURCES))

# Application name
PWD=$(shell pwd)
APPLICATION=$(shell echo '$(PWD)' | sed -e 's/-[^-/]*$$//' -e 's,^.*/,,')
APPFILE=ebin/$(APPLICATION).app
APPSRC=src/$(APPLICATION).app.src

ifdef COVER_REPORT
	COVER_REPORT:=$(REPORT_DIR)/$(APPLICATION)
else
	COVER_REPORT=cover_report
endif

# Lib applications that needs to be added to erl path
LIBDIRS?=../

VPATH=include:src

# Get the version
sinclude vsn.mk

# Make sure that just running "make" does what you expect
.PHONY: default_target
default_target: all

# This is necessary because of the way we call common.mk from the
# other makefiles:
export

MAKEFLAGS=sw
