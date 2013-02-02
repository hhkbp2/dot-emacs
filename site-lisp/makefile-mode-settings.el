;; -*- Emacs-Lisp -*-
;; Settings for `makefile-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-01-01 19:39>

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


(require 'make-mode)

;;; whitespace mode
;; show whitespace, tab, empty line, trailing whitespace ...
(require 'whitespace)

;; load face settings
(require 'makefile-face-settings)


(defun makefile-mode-settings()
  "Settings for `makefile-mode'."

  ;; show whitespace/tab
  (setq whitespace-style '(face tabs tab-mark trailing))
  (whitespace-mode 1))


(dolist (mode-hook
         '(makefile-mode-hook))
  (add-hook mode-hook
            'makefile-mode-settings))


(provide 'makefile-mode-settings)
