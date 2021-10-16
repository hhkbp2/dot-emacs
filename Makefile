# Makefile for managing my dot-emacs repository
#
# Author: Dylan.Wen <hhkbp2@gmail.com>
# Created: Sep 15, 2012
# Time-stamp: <2021-10-17 03:13>
#

QUIET     ?= @
#QUIET     ?= $(empty) $(empty)

RM        := rm -rf


all:


# $(call link-file source dest)
define link-file
  if [ -f "$2" ] || [ -L "$2" ] ; then \
      echo "$2 already exists!" > /dev/stderr && \
      exit 1; \
  else \
      ln -sf "$1" "$2" && \
      echo "link $1 to $2 done."; \
  fi
endef


.PHONY : all
all: notice

.PHONY : notice
notice:
	$(QUIET) echo "Dylan.Wen's dot-emacs repository."


.PHONY : site-init
site-init: link-early-init link-init

link-init:
	$(QUIET) $(call link-file,$(PWD)/dot_emacs,${HOME}/.emacs)

link-early-init:
	$(QUIET) $(call link-file,$(PWD)/early-init.el,${HOME}/.emacs.d/early-init.el)

# only for test
.PHONY : remove-links
remove-links:
	$(QUIET) $(RM) "${HOME}/.emacs" "${HOME}/.emacs.d/early-init.el"
