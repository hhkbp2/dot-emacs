;; -*- Emacs-Lisp -*-
;; Face settings for `smartparens'.

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-04-24 20:58>

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


(defun show-smartparens-face-settings ()
  "Face settings for `show-smartparens-mode'."
  (custom-set-faces
   '(sp-show-pair-match-face
     ((((class color) (min-colors 88))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 16))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "purple" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "purple" :foreground "white" :weight bold))
      (t (:background "#bb66ff" :foreground "#eeeeee" :weight bold))))
   '(sp-show-pair-mismatch-face
     ((((class color) (min-colors 88))
       (:background "#ff2f6a" :foreground "#232323" :weight bold))
      (((class color) (min-colors 16))
       (:background "#ff2f6a" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "red" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "red" :foreground "white" :weight bold))
      (t (:background "#ff2f6a" :foreground "#232323" :weight bold))))))

(defun smartparens-face-settings ()
  "Face settings for `smartparens-mode'."
  ;; TODO set these faces:
  ;; sp-highlight-pair-overlay
  ;; sp-highlight-wrap-overlay
  ;; sp-highlight-wrap-tag-overlay
  )


(provide 'smartparens-face-settings)
