;; -*- Emacs-Lisp -*-
;; Settings for `rainbow-delimiters'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-23 10:34>

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


(require 'rainbow-delimiters)
(require 'rainbow-delimiters-face-settings)


(defun rainbow-delimiters-settings ()
  "Settings for `rainbow-delimiters'."

  )

(eval-after-load "rainbow-delimiters"
  `(rainbow-delimiters-settings))


(defun enable-rainbow-delimiters ()
  (set-variable 'frame-background-mode 'dark)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(enable-rainbow-delimiters)


(provide 'rainbow-delimiters-settings)
