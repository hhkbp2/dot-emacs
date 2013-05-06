;; -*- Emacs-Lisp -*-
;; Settings for `skeleton'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-04-28 15:06>

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


(require 'skeleton)
(require 'dev-base)


(defun dw-auto-complete-pair ()
  "Automatically complete the right part of pairs used in all programming modes.
\(Except all lisp modes, for which there is a separat setting in `wlc'.
See `wlc/auto-complete-pair'.)
The pairs include '', \"\", [], (), {}."

  (interactive)

  (make-local-variable 'skeleton-pair)
  (make-local-variable 'skeleton-pair-on-word)
  (make-local-variable 'skeleton-pair-filter-function)
  (make-local-variable 'skeleton-pair-alist)

  ;; enable auto pair
  (setq skeleton-pair t)
  ;; don't auto complete pair before or inside a word
  (setq skeleton-pair-on-word nil)
  ;; enable no checking before inserting the complemental pair
  (setq skeleton-pair-filter-function (lambda () nil))
  ;; customize the complete part on each left part of pairs inserting
  (setq skeleton-pair-alist
        '((?' _ "'")
          (?\" _ "\"")
          (?\[ _ "]")
          (?\( _ ")")
          (?{ > _ ?})))
  ;; customize which key triggers the auto complete action
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))

(defun dw-auto-complete-indent-pair ()
  "Automatically complete the right part of pairs(with indentation)."

  (interactive)
  (dw-auto-complete-pair)
  ;; override the default {} pair auto-completion setting
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist
        (append '((?{ > \n > _ \n ?} >))
                (assq-delete-all '?{ skeleton-pair-alist))))

(defun skeleton-settings ()
  "Settings for `skeleton'."

  ;; 为部分编程mode加载右括号自动缩进补全
  ;; (dolist (mode-hook dev-mode-hook-list-static)
  ;;   (add-hook mode-hook 'dw-auto-complete-indent-pair))

  ;; 为除各种lisp mode外的编程mode加载右括号自动补全
  (dolist (mode-hook dev-mode-hook-list-nonlisp)
    (add-hook mode-hook 'dw-auto-complete-pair))
  )

(skeleton-settings)


(provide 'skeleton-settings)
