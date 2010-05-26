UTILS_DIR:=../../utils
NODES_DIR:=..

NODE_DIR:=$(shell pwd)
NODE:=$(shell basename "$(NODE_DIR)")

# Your own configuration goes in config.mk in the top level.
sinclude $(NODES_DIR)/config.mk
include $(NODES_DIR)/vsn.mk

LIBDIRS?=../../apps

BASENAME=$(NODE)-$(NODEVSN)
REL="$(UTILS_DIR)/erlrel"

.PHONY: default_target
default_target: target

# Avoid warning about missing config.mk (fatal error in GNU Make 3.80)
$(NODES_DIR)/config.mk: ;

export

MAKEFLAGS=sw
