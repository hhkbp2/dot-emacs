;; -*- Emacs-Lisp -*-
;; Settings for `erlang-mode'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-10-25 10:47>

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


(defun erlang-mode-settings ()
  "Settings for `erlang-mode'."

  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")

  (dolist (file-mode-pattern '(;; application description file
                               ("\\.app\\'" . erlang-mode)
                               ;; release description file
                               ("\\.rel\\'" . erlang-mode)
                               ;; release configuration file
                               ("\\.config\\'" . erlang-mode)))
    (add-to-list 'auto-mode-alist file-mode-pattern))

  (local-set-key [(control c) (c)] 'comment-dwim)
  (local-set-key [(control c) (control c)] 'comment-dwim)
  )

(add-hook 'erlang-mode-hook
          'erlang-mode-settings)

(provide 'erlang-mode-settings)
