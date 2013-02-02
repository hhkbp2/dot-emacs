;; -*- Emacs-Lisp -*-
;; Face settings for `paren'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 13:18>

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


(require 'paren)


(defun paren-face-settings ()
  "Face settings for `paren'."
  (custom-set-faces
   '(show-paren-match
     ((((class color) (min-colors 88))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 16))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "purple" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "purple" :foreground "white" :weight bold))
      (t (:background "#bb66ff" :foreground "#eeeeee" :weight bold))))
   '(show-paren-mismatch
     ((((class color) (min-colors 88))
       (:background "#ff2f6a" :foreground "#232323" :weight bold))
      (((class color) (min-colors 16))
       (:background "#ff2f6a" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "red" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "red" :foreground "white" :weight bold))
      (t (:background "#ff2f6a" :foreground "#232323" :weight bold))))))


(eval-after-load "paren"
  '(paren-face-settings))


(provide 'paren-face-settings)