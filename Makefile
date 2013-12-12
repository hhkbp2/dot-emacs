# Makefile for managing my dot-emacs repository
#
# Author: Dylan.Wen <hhkbp2@gmail.com>
# Created: Sep 15, 2012
# Time-stamp: <2013-12-13 21:36>
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
	$(QUIET) cd import && tar -xjf auto-complete-1.3.tar.bz2 && cd auto-complete-1.3 && make
	-$(QUIET) cd import && tar -xzf color-theme-6.6.0.tar.gz && cd color-theme-6.6.0 && make
	$(QUIET) cd import && tar -xzf ecb-2.40.tar.gz && \
       sed -i -e "s#\(ecb-required-cedet-version-max '(1 \)0#\11#" ./ecb-2.40/ecb-upgrade.el
	$(QUIET) cd import && unzip jdee-bin-2.4.0.1.zip && \
      sed -i -e 's#\(jde-cedet-max-version "1\.\)0#\11#' ./jdee-2.4.0.1/lisp/jde.el
	$(QUIET) cd import && tar -xjf workgroups-experimental-f3339422.tar.bz2
	$(QUIET) cd import && tar -xzf distel-4.03.tar.gz

link-dot-emacs:
	$(QUIET) $(call link-dot-emacs-if-needs)



.PHONY : site-clean
site-clean:
	$(QUIET) cd import && $(RM) \
  auto-complete-1.3 \
  cedet-1.0 cedet-1.1 cedet \
  color-theme-6.6.0 \
  ecb-2.40 \
  jdee-2.4.0.1 \
  workgroups \
  distel-4.03

