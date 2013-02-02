;; -*- Emacs-Lisp -*-
;; Settings for `emacs-lisp-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-01-18 14:57>

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


(defun emacs-lisp-mode-settings ()
  "Settings for `emacs-lisp-mode'."

  ;; enter the default elisp debugger: debug, when error happends.
  (setq debug-on-error t)

  ;; pause a few seconds at each break point
  ;; when in edebug `trace' or `continue' mode.
  ;; rather that just 1 second by default
  ;; because probably I need some more time on each break.
  (setq edebug-sit-for-seconds 2)

  ;; enable sexp evaluation.
  (put 'eval-expression 'disable nil)

  ;; key bindings
  ;; 写elisp经常用到`edebug-defun'
  (define-key emacs-lisp-mode-map [(control c) (f)] 'edebug-defun)
  (define-key emacs-lisp-mode-map [(control c) (g)] 'edebug-all-defs)
  )

(emacs-lisp-mode-settings)


(provide 'emacs-lisp-mode-settings)