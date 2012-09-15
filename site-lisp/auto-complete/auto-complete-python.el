;; auto-complete-python.el --- Settings for `auto-complete' in python mode.
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-12-18 23:25>

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


(require 'auto-complete)
(require 'auto-complete-config)
(require 'python)
(require 'util)


(defun ac-pythondotel-candidates ()
  (mapcar '(lambda (completion)
            (first (last (split-string completion "\\." t))))
          (python-symbol-completions (python-completion-at-point))))


(ac-define-source pythondotel
  '((candidates . ac-pythondotel-candidates)
    (symbol     . "o")))


(defun ac-ropemacs+-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (ignore-errors
            (rope-completions))))


(ac-define-source ropemacs+
  '((candidates . ac-ropemacs+-candidates)
    (symbol     . "p")))


(ac-define-source ropemacs+-dot
  '((candidates . ac-ropemacs+-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))


(defun ac-settings-4-python ()
  (setq ac-sources

        ;; one alternative: add ropemacs source to default ac sources
        ;;(append (list 'ac-source-ropemacs) ac-sources)

        ;; another alternative:
        ;; specify all valid sources for python mode one by one
        '(
          ;;ac-source-pythondotel
          ac-source-ropemacs
          ;; ac-source-ropemacs+
          ;; ac-source-ropemacs+-dot
          ac-source-semantic
          ac-source-yasnippet
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))


(defun auto-complete-python ()
  ;; setup auto-complete
  (ac-ropemacs-setup)
  (ac-settings-4-python))


(apply-args-list-to-fun
 (lambda (hook fun)
   (am-add-hooks hook fun))
 `(('python-mode-hook 'auto-complete-python)))


(provide 'auto-complete-python)

;;; auto-complete-python.el ends here
