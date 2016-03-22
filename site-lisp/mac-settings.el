;;; mac-settings.el --- Settings for mac os
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:31>

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


(require 'exec-path-from-shell)
(require 'dash-at-point-settings)


(defun mac-settings ()
  "Settings for mac os."

  ;; set `exec-path' and PATH to the shell path rather than
  ;; the system-wide default
  (exec-path-from-shell-initialize))


(provide 'mac-settings)

;;; mac-settings.el ends here
