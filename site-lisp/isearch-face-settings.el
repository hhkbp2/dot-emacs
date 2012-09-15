;; -*- Emacs-Lisp -*-
;; Face settings for `isearch'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 14:17>

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


(defun isearch-face-settings ()
  "Face settings for `isearch'."
  (custom-set-faces
   '(isearch
     ((((class color) (min-colors 88))
       (:background "#fce94f" :foreground "#232323"))
      (((class color) (min-colors 16))
       (:background "#fce94f" :foreground "#232323"))
      (((class color) (min-colors 8))
       (:background "yellow" :foreground "black"))
      (((type tty) (class mono))
       (:background "yellow" :foreground "black"))
      (t (:background "#fce94f" :foreground "#232323"))))
   '(isearch-fail
     ((((class color) (min-colors 88))
       (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))
      (((class color) (min-colors 16))
       (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))
      (((class color) (min-colors 8))
       (:background "red" :foreground "yellow" :weight bold))
      (((type tty) (class mono))
       (:background "red" :foreground "yellow" :weight bold))
      (t (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))))
   '(lazy-highlight ((t (:background "paleturquoise4")))))
  )


(eval-after-load "isearch"
  '(isearch-face-settings))


(provide 'isearch-face-settings)