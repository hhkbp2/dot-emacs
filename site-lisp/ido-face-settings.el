;; -*- Emacs-Lisp -*-
;; Face settings for `ido'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 14:57>

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


(defun ido-face-settings ()
  "Face settings for `ido'."
  (custom-set-faces
   '(ido-first-match ((t (:weight bold))))
   '(ido-incomplete-regexp ((t (:foreground "Pink" :weight bold))))
   '(ido-indicator
     ((t (:background "red" :foreground "yellow1" :width condensed))))
   '(ido-only-match
     ((((class color) (min-colors 88)) (:foreground "#00c900"))
      (((class color) (min-colors 16)) (:foreground "#00c900"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (((type tty) (class mono)) (:foreground "green"))
      (t (:foreground "#00c900"))))
   '(ido-subdir
     ((((class color) (min-colors 88))
       (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 16))
       (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 8))
       (:foreground "blue" :weight bold))
      (((type tty) (class mono))
       (:foreground "blue" :weight bold))
      (t (:foreground "#9e91ff" :weight bold)))))
  )


(eval-after-load "ido"
  '(ido-face-settings))


(provide 'ido-face-settings)