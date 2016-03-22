;;; python-base-settings.el --- A few base settings for python
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:49>

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


(defconst dw-python-dev-dir (expand-file-name "~/pro/python")
  "Personal development code directory of python.")

(defconst dw-python-path (expand-file-name
                          "~/local/lib/python2.7/site-packages/")
  "Personal PYTHONPATH for addtional libraries.")

(defun dw-prepare-pypath ()
  (let ((pypath (getenv "PYTHONPATH")))
    (if (or (null pypath) (string= "" pypath))
        (setenv "PYTHONPATH" dw-python-path)
      (if (not (search dw-python-path pypath))
          (setenv "PYTHONPATH" (concat dw-python-path ":" pypath))))))

(when (eq system-type 'darwin)
  ;; prepare PYTHONPATH for mac emacs as app started on dock
  (dw-prepare-pypath))


(provide 'python-base-settings)

;;; python-base-settings.el ends here
