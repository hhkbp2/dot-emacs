;; -*- Emacs-Lisp -*-
;; Face settings for `linum-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-08-26 23:21>

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

(require 'linum)
(require 'linum-relative)


(defun linum-face-settings ()
  "Face settings for `linum-mode'."
  (custom-set-faces
   '(linum
      ((((class color) (min-colors 88))
        (:background "#bbbbbb" :foreground "#555753"
                     :weight normal :slant normal))
       (((class color) (min-colors 16))
        (:background "#bbbbbb" :foreground "#555753"
                     :weight normal :slant normal))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"
                     :weight normal :slant normal))
       (((type tty) (class mono))
        (:background "white" :foreground "black"
                     :weight normal :slant normal))
       (t (:background "#bbbbbb" :foreground "#555753"
                       :weight normal :slant normal))))))


(defun linum-relative-face-settings ()
  "Face settings for `linum-relative'."
  (custom-set-faces
   '(linum-relative-current-line-face
     ((((class color) (min-colors 88))
        (:background "gray90" :foreground "#555753"
                     :weight semi-bold :slant normal))
       (((class color) (min-colors 16))
        (:background "gray90" :foreground "#555753"
                     :weight semi-bold :slant normal))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"
                     :weight semi-bold :slant normal))
       (((type tty) (class mono))
        (:background "white" :foreground "black"
                     :weight semi-bold :slant normal))
       (t (:background "gray90" :foreground "#555753"
                       :weight semi-bold :slant normal))))))


(eval-after-load 'linum
  '(linum-face-settings))

(eval-after-load 'linum-relative
  '(linum-relative-face-settings))


(provide 'linum-face-settings)
