;; -*- Emacs-Lisp -*-
;; Settings for `whitespace'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-01-01 22:17>

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


(require 'whitespace)


(defun whitespace-face-settings ()
  "Face settings for `whitespace'."
  (custom-set-faces
   '(whitespace-space
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-space-before-tab
     ((t (:background "DarkOrange" :foreground "#ff79d9"))))
   '(whitespace-space-after-tab
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-hspace
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-tab
     ((((type x)) (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 256))
       (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 16))
       (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 8))
       (:background "white" :foreground "red"))
      (((type tty) (class mono))
       (:background "white" :foreground "red"))
      (t (:background "gray30" :foreground "#ff79d9"))))
   '(whitespace-newline
     ((t (:foreground "darkgray" :weight normal))))
   '(whitespace-trailing
     ((((type x)) (:background "gray30"))
      (((class color) (min-colors 256)) (:background "gray30"))
      (((class color) (min-colors 16)) (:background "gray30"))
      (((class color) (min-colors 8)) (:background "gray30"))
      (t (:background "gray30"))))
   '(whitespace-line
     ((t (:background "gray22" :foreground "violet"))))
   '(whitespace-indentation
     ((((type x)) (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 256))
       (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 16))
       (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 8))
       (:background "white" :foreground "red"))
      (t (:background "gray40" :foreground "#ff79d9"))))
   '(whitespace-empty
     ((t (:background "gray22" :foreground "#ff79d9"))))
   )
  )

(eval-after-load "whitespace"
  `(whitespace-face-settings))


(provide 'whitespace-face-settings)
