;;; dev-base-settings.el --- A few base definations and contants of dev
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:06>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'cl-lib)


(defvar usual-mode-hook-list
  '(dired-mode-hook
    ibuffer-mode-hook
    find-file-hook
    diff-mode-hook
    help-mode-hook
    apropos-mode-hook
    completion-list-mode-hook
    Man-mode-hook))


(defvar dev-mode-list-nonlisp
  '(;; c/c++
    ;;c-mode-common
    c-mode
    c++-mode
    ;; makefile
    makefile-mode
    makefile-gmake-mode
    ;; java
    java-mode
    ;; shell, term and shell script
    sh-mode
    shell-mode
    ;; awk
    awk-mode
    ;; ruby
    ruby-mode
    enh-ruby-mode
    ;; python
    python-mode
    ;; erlang
    erlang-mode
    elixir-mode
    ;; rust
    rust-mode
    ;; go
    go-mode
    ;; javascript
    js-mode
    ;; snippet-mode
    snippet-mode
    ;; latex
    latex-mode
    LaTeX-mode
    ;; html
    html-mode
    ;; xml
    xml-mode
    sgml-mode
    ;; markdown
    markdown-mode
    ;; sql
    sql-mode
    ;; conf-mode
    conf-mode
    ;; text
    text-mode
    )
  "A list of non-lisp dev modes.")


(defvar dev-mode-list-lisp
  '(emacs-lisp-mode
    lisp-mode
    scheme-mode
    clojure-mode
    slime-repl-mode
    joxa-mode)
  "A list of lisp modes.")


(defvar dev-mode-list
  (cl-union dev-mode-list-lisp dev-mode-list-nonlisp)
  "A list of all dev modes.")


(defvar dev-mode-hook-list-nonlisp
  '(;; c/c++
    c-mode-common-hook
    ;; makefile
    makefile-mode-hook
    ;; java
    java-mode-hook
    ;; shell, term and shell script
    sh-mode-hook
    shell-mode-hook
    ;; awk
    awk-mode-hook
    ;; ruby
    ruby-mode-hook
    enh-ruby-mode-hook
    ;; python
    python-mode-hook
    ;; erlang
    erlang-mode-hook
    elixir-mode-hook
    ;; rust
    rust-mode-hook
    ;; go
    go-mode-hook
    ;; javascript
    js-mode-hook
    ;; snippet-mode
    snippet-mode-hook
    ;; latex
    latex-mode-hook
    LaTeX-mode-hook
    ;; html
    html-mode-hook
    ;; markdown
    markdown-mode-hook
    ;; conf-mode
    conf-mode-hook
    ;; text
    test-mode-hook
    )
  "A list of non-lisp dev mode hooks.")


(defvar dev-mode-hook-list-static
  '(c-mode-common-hook
    java-mode-hook))


(defvar dev-mode-hook-list-lisp
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    scheme-mode-hook
    clojure-mode-hook
    slime-repl-mode-hook
    joxa-mode-hook)
  "A list of lisp mode hooks.")


(defvar dev-mode-hook-list
  (cl-union dev-mode-hook-list-lisp dev-mode-hook-list-nonlisp)
  "A list of all dev mode hooks.")


(provide 'dev-base-settings)

;;; dev-base-settings.el ends here
