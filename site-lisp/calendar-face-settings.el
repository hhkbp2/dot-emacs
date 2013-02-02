;; -*- Emacs-Lisp -*-
;; Face settings for `calendar'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 14:02>

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


(defun calendar-face-settings ()
  "Face settings for `calendar'."
  (custom-set-faces
   '(calendar-today
     ((((class color) (min-colors 88))
       (:background "black" :foreground "#ff9900" :underline t))
      (((class color) (min-colors 16))
       (:background "black" :foreground "#ff9900" :underline t))
      (((class color) (min-colors 8))
       (:background "white" :foreground "yellow"))
      (((type tty) (class mono))
       (:background "white" :foreground "yellow"))
      (t (:background "black" :foreground "#ff9900"))))
   '(diary ((t (:foreground "yellow1"))))
   '(holiday
     ((((class color) (min-colors 88))
       (:background "gray30" :foreground "#acc900" :slant italic))
      (((class color) (min-colors 16))
       (:background "gray30" :foreground "#acc900" :slant italic))
      (((class color) (min-colors 8))
       (:background "white" :foreground "green" :slant italic))
      (((type tty) (class mono))
       (:background "white" :foreground "green" :slant italic))
      (t (:background "gray30" :foreground "#acc900" :slant italic)))))
  )


(eval-after-load "calendar"
  '(calendar-face-settings))


(provide 'calendar-face-settings)