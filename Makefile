# Makefile for managing my dot-emacs repository
#
# Author: Dylan.Wen <hhkbp2@gmail.com>
# Created: Sep 15, 2012
# Time-stamp: <2020-08-23 15:48>
#

QUIET     := @
#QUIET      := $(empty) $(empty)

RM        := rm -rf


all:


# (call link-dot-emacs-if-needs)
define link-dot-emacs-if-needs
  if [ -f "${HOME}/.emacs" ] && [ ! -h "${HOME}/.emacs" ] ; then \
      echo "~/.emacs exists as a file!" > /dev/stderr && \
      exit 1; \
  else \
      ln -sf $(PWD)/dot_emacs "${HOME}/.emacs" && \
      echo "link dot_emacs to ~/.emacs done."; \
  fi;
endef


.PHONY : all
all: notice

.PHONY : notice
notice:
	$(QUIET) echo "Dylan.Wen's dot-emacs repository."


.PHONY : site-init
site-init: link-dot-emacs

link-dot-emacs:
	$(QUIET) $(call link-dot-emacs-if-needs)

