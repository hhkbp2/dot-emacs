;;; color-theme-settings.el --- Setting for `color-theme'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:06>

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
;;
;; `color-theme'
;; various color themes for emacs

;;; Code:


(require 'color-theme)
(require 'color-theme+)

(defun color-theme-settings ()
  "Settings for `color-theme'."

  (color-theme-initialize)
  )


(eval-after-load "color-theme"
  `(color-theme-settings))


(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
