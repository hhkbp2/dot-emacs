;; -*- Emacs-Lisp -*-
;; Face settings for `speedbar'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 13:09>

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


(require 'speedbar-loaddefs)


(defun speedbar-face-settings ()
  "Face settings for `speedbar'."
  (custom-set-faces
   '(speedbar-button-face
     ((((class color) (min-colors 88)) (:foreground "dark red"))
      (((class color) (min-colors 16)) (:foreground "dark red"))
      (((class color) (min-colors 8)) (:foreground "red"))
      (((type tty) (class mono)) (:foreground "red"))
      (t (:foreground "dark red"))))
   '(speedbar-directory-face
     ((((class color) (min-colors 88)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 16)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
      (((type tty) (class mono)) (:foreground "blue" :weight bold))
      (t (:foreground "#9e91ff" :weight bold))))
   '(speedbar-file-face
     ((((class color) (min-colors 88)) (:foreground "#eeeeee"))
      (((class color) (min-colors 16)) (:foreground "#eeeeee"))
      (((class color) (min-colors 8)) (:foreground "white"))
      (((type tty) (class mono)) (:foreground "white"))
      (t (:foreground "#eeeeee"))))
   '(speedbar-highlight-face
     ((((class color) (min-colors 88)) (:background "#555753"))
      (((class color) (min-colors 16)) (:background "#555753"))
      (((class color) (min-colors 8)) (nil))
      (((type tty) (class mono)) (nil))
      (t (:background "#555753"))))
   '(speedbar-selected-face
     ((((class color) (min-colors 88)) (:foreground "#009cff"))
      (((class color) (min-colors 16)) (:foreground "#009cff"))
      (((class color) (min-colors 8)) (:foreground "blue"))
      (((type tty) (class mono)) (:foreground "blue"))
      (t (:foreground "#009cff"))))
   '(speedbar-separator-face
     ((t (:background "blue" :foreground "white" :overline "gray"))))
   '(speedbar-tag-face
     ((((class color) (min-colors 88))
       (:foreground "#acc900" :weight book))
      (((class color) (min-colors 16))
       (:foreground "#acc900" :weight book))
      (((class color) (min-colors 8))
       (:foreground "green" :weight book))
      (((type tty) (class mono))
       (:foreground "green" :weight book))
      (t (:foreground "#acc900" :weight book))))))


(eval-after-load "speedbar"
  `(speedbar-face-settings))


(provide 'speedbar-face-settings)
