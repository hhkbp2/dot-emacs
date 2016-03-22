;;; dired-x-settings.el --- Settings for `dired-x'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:12>

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

;; dired-x, 忽略不感兴趣的文件

(autoload 'dired-omit-mode "dired-x"
  "Toggle Dired-Omit mode.
With numeric ARG, enable Dired-Omit mode if ARG is positive, disable
otherwise.  Enabling and disabling is buffer-local.
If enabled, \"uninteresting\" files are not listed.
Uninteresting files are those whose filenames match regexp `dired-omit-files',
plus those ending with extensions in `dired-omit-extensions'."
  t)

(am-add-hooks
 `(dired-mode-hook)
 'dired-omit-mode)

(defun dired-x-settings ()
  "Settings for `dired-x'."
  (unless is-before-emacs-21
    (setq dired-omit-files (concat dired-omit-files "\\|^\\.\\|^semantic.cache$\\|^CVS$"))
    (if mswin
        (setq dired-omit-files (concat dired-omit-files "\\|^_"))))
  (setq dired-omit-size-limit 1000000))

(eval-after-load "dired-x"
  `(dired-x-settings))

(provide 'dired-x-settings)

;;; dired-x-settings.el ends here
