;;; flycheck-settings.el --- Settings for `flycheck'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-25 10:53>

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


(require 'flycheck)


(defun flycheck-4-elisp ()
  "Flycheck settings for `emacs-lisp-mode'."
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(defun flycheck-4-erlang ()
  "Flycheck settings for `erlang-mode'."
  ;; TODO customize
  ;; flycheck-erlang-include-path
  ;; flycheck-erlang-library-path
  (add-hook 'erlang-mode-hook 'flycheck-mode)
  )

(defun flycheck-4-elixir ()
  "Flycheck settings for `elixir-mode'."

  ;; add flycheck checker for elixir
  (flycheck-define-checker elixir-mix
    "An Elixir syntax checker using the Elixir interpreter.
Refer to `https://github.com/ananthakumaran/dotfiles/.emacs.d/init-elixir.el'."
    :command ("mix" "compile" source)
    :error-patterns ((error line-start
                            "** (" (zero-or-more not-newline) ") "
                            (zero-or-more not-newline) ":" line ": " (message)
                            line-end)
                     (warning line-start
                              (one-or-more (not (syntax whitespace))) ":"
                              line ": "
                              (message)
                              line-end))
    :modes elixir-mode)
  (add-to-list 'flycheck-checkers 'elixir-mix)
  (add-hook 'elixir-mode-hook 'flycheck-mode))


(defun flycheck-4-python ()
  "Flycheck settings for `python-mode'."
  (add-hook 'python-mode-hook '(lambda ()
                                 (flycheck-mode)
                                 (flycheck-select-checker 'python-pylint))))


(defun flycheck-4-go ()
  "Flycheck settings for `go-mode'."
  (require 'go-flycheck)
  (add-hook 'go-mode-hook 'flycheck-mode))


(defun flycheck-4-rust ()
  "Flycheck settings for `rust-mode'."
  (require 'flycheck-rust)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(defun flycheck-settings ()
  "Settings for `flycheck'."

  ;; check syntax when
  ;; 1. `flycheck-mode' is enabled
  ;; 2. the buffer is save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  ;; enable flycheck in specified modes
  (dolist (func '(flycheck-4-elisp
                  flycheck-4-erlang
                  flycheck-4-elixir
                  flycheck-4-python
                  flycheck-4-go
                  flycheck-4-rust))
    (funcall func))
  )


(eval-after-load "flycheck"
  `(flycheck-settings))


(provide 'flycheck-settings)

;;; flycheck-settings.el ends here
