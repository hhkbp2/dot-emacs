;; -*- Emacs-Lisp -*-
;; An enhancement of `color-theme'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-01-29 00:03>

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


(require 'color-theme)


(defgroup color-theme+ nil
  "An enhancement of `color-theme'."
  :group 'faces)


;; redifine the function `color-theme-spec-compat' of `color-theme'
(if (or (featurep 'xemacs)
        (< emacs-major-version 21))
    (defalias 'color-theme-spec-compat 'identity)
(defun color-theme-spec-compat (spec)
  "An redifination of `color-theme-spec-compat' of `color-theme'.
To apply different face settings according to different term types."
  :group 'color-theme+
  (let (compat-spec props)
    (dolist (dis-atts spec)
      (setq props (cadr dis-atts))
      (when (plist-member props :bold)
        (setq props (color-theme-plist-delete props :bold))
        (unless (plist-member props :weight)
          (setq props (plist-put props :weight 'bold))))
      (when (plist-member props :italic)
        (setq props (color-theme-plist-delete props :italic))
        (unless (plist-member props :slant)
          (setq props (plist-put props :slant 'italic))))
      (setcdr dis-atts `(,props))
      (add-to-list 'compat-spec dis-atts 'APPEND))
    compat-spec)))


;; (color-theme-spec-compat '((t (:foreground "blue" :bold t))))
;; (color-theme-spec-compat
;; '((t (:bold t :foreground "blue" :weight extra-bold))))
;; (color-theme-spec-compat '((t (:italic t :foreground "blue"))))
;; (color-theme-spec-compat
;; '((t (:slant oblique :italic t :foreground "blue"))))
;; (color-theme-spec-compat
;; '((((class color) (min-colors 88) (background dark))
;;    (:bold t :foreground "#96ff00"))
;;   (((class color) (min-colors 88) (background light))
;;    (:bold t :foreground "#96ff00"))
;;   (((class color) (min-colors 16) (background dark))
;;    (:foreground "limegreen" :italic))
;;   (((class color) (min-colors 16) (background light))
;;    (:foreground "limegreen" :italic))
;;   (((class color) (min-colors 8) (background dark))
;;    (:foreground "green"))
;;   (((class color) (min-colors 8) (background light))
;;    (:foreground "green"))
;;   (((type tty) (class mono)) (:foreground "green"))
;;   (t (:foreground "#96ff00"))))


(provide 'color-theme+)