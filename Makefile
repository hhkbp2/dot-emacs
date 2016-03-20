# Makefile for managing my dot-emacs repository
#
# Author: Dylan.Wen <hhkbp2@gmail.com>
# Created: Sep 15, 2012
# Time-stamp: <2016-03-20 11:44>
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
site-init: init-packages link-dot-emacs

.PHONY : init-packages
init-packages:
	$(QUIET) cd import && tar -xzf ecb-2.40.tar.gz && \
       sed -i -e "s#\(ecb-required-cedet-version-max '(1 \)0#\11#" ./ecb-2.40/ecb-upgrade.el
	$(QUIET) cd import && tar -xzf distel-4.03.tar.gz

link-dot-emacs:
	$(QUIET) $(call link-dot-emacs-if-needs)



.PHONY : site-clean
site-clean:
	$(QUIET) cd import && $(RM) \
  ecb-2.40 \
  distel-4.03

