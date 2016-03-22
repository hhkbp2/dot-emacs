;;; erlang-mode-settings.el --- Settings for `erlang-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:30>

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


(require 'erlang-start)
(require 'dw-functionals)
(require 'distel)



;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(defun distel-settings ()
  "Settings for `distel'."

  (add-hook 'erlang-shell-mode-hook
            (lambda ()
              ;; add some Distel bindings to the Erlang shell
              (dolist (spec distel-shell-keys)
                (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
  )


(defun erlang-mode-settings ()
  "Settings for `erlang-mode'."

  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))


  (dw-hungry-delete-on-mode-map erlang-mode-map)
  (dw-commet-dwin-on-mode-map erlang-mode-map)

  (add-hook 'erlang-mode-hook
            (lambda ()
              ;; add Erlang functions to an imenu menu
              (imenu-add-to-menubar "imenu")
              (local-set-key [(control c) (m) (f)] 'mark-erlang-function)
              (local-set-key [(control c) (m) (c)] 'mark-erlang-clause)
              ;; set indentations
              (setq erlang-indent-level 2
                    erlang-indent-guard 4
                    erlang-argument-indent 4)
              ))
  (distel-setup)
  )

(eval-after-load "erlang"
  '(erlang-mode-settings))

(eval-after-load "distel"
  '(distel-settings))


(dw-add-file-mode-pattern-list '(;; application description file
                                 ("\\.app$" . erlang-mode)
                                 ;; release description file
                                 ("\\.rel$" . erlang-mode)
                                 ;; release configuration file
                                 ("\\.config$" . erlang-mode)))


(provide 'erlang-mode-settings)

;;; elang-mode-settings.el ends here
