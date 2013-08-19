;; -*- Emacs-Lisp -*-
;; Settings for `pycomplete'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-19 11:04>

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


(require 'pycomplete)


(defun pycomplete-settings ()
  "Settings for `pycomplete'."

  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))

  (setq py-python-command "python")
  )


(eval-after-load "pycomplete"
  `(pycomplete-settings))


(provide 'pycomplete-settings)
