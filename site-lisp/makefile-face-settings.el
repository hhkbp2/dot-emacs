;;; makefile-face-settings.el --- Face settings for `makefile-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:32>

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


(defun makefile-face-settings ()
  "Face settings for `makefile-mode'."
  (custom-set-faces
   '(makefile-targets ((t (:foreground "#009cff" :weight semi-bold))))
   '(makefile-space ((t (:background "#ff79d9"))))
   '(makefile-shell
     ((((class color) (min-colors 88))
       (:foreground "wheat" :weight book))
      (((class color) (min-colors 16))
       (:foreground "wheat" :weight book))
      (((class color) (min-colors 8))
       (:foreground "yellow"))
      (((type tty) (class mono))
       (:foreground "yellow"))
      (t (:foreground "wheat" :weight book))))
   '(makefile-makepp-perl ((t (:background "DarkBlue")))))
  )


(eval-after-load "make-mode"
  '(makefile-face-settings))


(provide 'makefile-face-settings)

;;; makefile-face-settings.el ends here
