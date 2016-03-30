;;; package-settings.el --- Settings for the package manager `package'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-30 10:55>

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

(require 'my-subdirs)

;; After loading the init file and abbrev file(if there is any),
;; emacs 24 will run some code like these to load installed packages.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))
;; And then variable `package-archives' will be set to the default value
;; '(("gnu" . "http://elpa.gnu.org/packages/"))


(defvar dw-package-addition-sources
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/"))
  "Addtional package sources.")

(defvar dw-package-list
  '(
    undo-tree
    redo+

    color-theme
    cal-china-x

    dired+
    dired-details
    dired-details+
    fiplr
    popwin
    neotree
    sr-speedbar
    windresize

    ido-complete-space-or-hyphen
    ido-ubiquitous
    helm
    helm-ag
    helm-swoop
    helm-descbinds
    helm-describe-modes
    smex

    google-this
    youdao-dictionary
    guide-key

    auto-complete
    ac-ispell
    yasnippet
    ecb
    gtags
    flycheck
    highlight-indentation
    highlight-symbol
    highlight-tail
    multi-term

    exec-path-from-shell
    dash-at-point
    osx-dictionary

    rainbow-delimiters
    smartparens
    slime
    geiser
    ac-geiser

    clojure-mode
    cider

    google-c-style

    erlang
    elixir-mode

    python-mode
    pymacs
    jedi
    python-pep8
    python-pylint

    enh-ruby-mode

    rust-mode
    racer
    ac-racer
    flycheck-rust

    go-mode
    go-autocomplete
    go-eldoc
    golint
    go-errcheck

    thrift
    protobuf-mode
    scss-mode
    yaml-mode
    toml-mode
    json-mode
    ))

(defun dw-package-recompile-all ()
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun dw-package-check-all-installed ()
  (interactive)
  (dolist (p dw-package-list)
    (when (not (package-installed-p p))
      (package-install p))))

(defun package-settings ()
  (dolist (source dw-package-addition-sources)
    (add-to-list 'package-archives source))
  (my-add-subdirs-to-load-path package-user-dir))


(eval-after-load "package"
  `(package-settings))


(provide 'package-settings)

;;; package-settings.el ends here
