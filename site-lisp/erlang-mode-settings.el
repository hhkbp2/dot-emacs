;; -*- Emacs-Lisp -*-
;; Settings for `erlang-mode'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-11-15 16:32>

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
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")

  (local-set-key "\177" 'c-hungry-backspace)
  (local-set-key [backspace] 'c-hungry-backspace)
  (local-set-key [deletechar] 'c-hungry-delete-forward)
  (local-set-key [delete] 'c-hungry-delete-forward)
  (local-set-key [(control d)] 'c-hungry-delete-forward)

  (local-set-key [(control c) (c)] 'comment-dwim)
  (local-set-key [(control c) (control c)] 'comment-dwim)

  (local-set-key [(control c) (m) (f)] 'mark-erlang-function)
  (local-set-key [(control c) (m) (c)] 'mark-erlang-clause)

  (distel-settings)
  )

(add-hook 'erlang-mode-hook
          'erlang-mode-settings)
(distel-setup)


(dw-add-file-mode-pattern-list '(;; application description file
                                 ("\\.app$" . erlang-mode)
                                 ;; release description file
                                 ("\\.rel$" . erlang-mode)
                                 ;; release configuration file
                                 ("\\.config$" . erlang-mode)))


(provide 'erlang-mode-settings)
