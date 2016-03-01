;; -*- Emacs-Lisp -*-
;; Settings for the package manager `package'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-01 12:18>

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

(require 'my-subdirs)

;; After loading the init file and abbrev file(if there is any),
;; emacs 24 will run some code like these to load installed packages.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))
;; And then variable `package-archives' will be set to the default value
;; '(("gnu" . "http://elpa.gnu.org/packages/"))


(defvar dw-package-addition-sources
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/"))
  "Addtional package sources.")

(defun dw-package-recompile-all ()
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun package-settings ()
  (dolist (source dw-package-addition-sources)
    (add-to-list 'package-archives source))
  (my-add-subdirs-to-load-path package-user-dir))


(eval-after-load "package"
  `(package-settings))


(provide 'package-settings)
