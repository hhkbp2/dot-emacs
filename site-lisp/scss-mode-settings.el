;;; scss-mode-settings.el --- Settings for `scss-mode'
;; -*- Emacs-Lisp -*-;

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 15:32>

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

(defun scss-mode-settings()
  "Settings for `scss-mode'."

  ;; by default `scss-mode' will auto-compile scss file and create a responding .css file.
  ;; disable it
  (setq scss-compile-at-save nil)
  )

(eval-after-load "scss-mode"
  '(scss-mode-settings))

(provide 'scss-mode-settings)

;;; scss-mode-settings.el ends here
