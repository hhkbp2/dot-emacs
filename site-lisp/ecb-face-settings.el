;; -*- Emacs-Lisp -*-
;; Face settings for `ecb'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-04-09 01:35>

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


(defun ecb-face-settings ()
  "Face settings for `ecb'."
  (custom-set-faces
   '(ecb-analyse-bucket-element-face ((t (:height 1.0))))
   '(ecb-analyse-bucket-node-face ((t (:weight bold :height 1.0))))
   '(ecb-analyse-face ((t (:background "#555753"))))
   '(ecb-analyse-general-face ((t (:height 1.0))))
   '(ecb-bucket-node-face ((t (:weight bold :height 1.0))))
   '(ecb-default-general-face ((t (:height 1.0))))
   '(ecb-default-highlight-face ((t (:background "#555753"))))
   '(ecb-directories-general-face ((t (:height 1.0))))
   '(ecb-directory-face ((t (:background "#555753"))))
   '(ecb-directory-not-accessible-face
      ((t (:foreground "gray60" :height 1.0))))
   '(ecb-history-bucket-node-face ((t (:weight bold :height 1.0))))
   '(ecb-history-dead-buffer-face ((t (:foreground "gray60" :height 1.0))))
   '(ecb-history-face ((t (:background "#555753"))))
   '(ecb-history-general-face ((t (:height 1.0))))
   '(ecb-history-indirect-buffer-face ((t (:slant italic :height 1.0))))
   '(ecb-method-face ((t (:background "#555753"))))
   '(ecb-method-non-semantic-face ((t (:foreground "brown" :height 1.0))))
   '(ecb-methods-general-face ((t (:height 1.0))))
   '(ecb-mode-line-data-face ((t (nil))))
   '(ecb-mode-line-prefix-face ((t (:foreground "forestgreen"))))
   '(ecb-mode-line-win-nr-face ((t (:weight bold))))
   '(ecb-source-face ((t (:background "#555753"))))
   '(ecb-source-in-directories-buffer-face
      ((t (:foreground "lightblue1" :height 1.0))))
   '(ecb-source-read-only-face ((t (:slant italic :height 1.0))))
   '(ecb-sources-general-face ((t (:height 1.0))))
   '(ecb-tag-header-face ((t (:background "gray40"))))
   '(ecb-tree-guide-line-face ((t (:foreground "gray" :height 1.0))))
   '(ecb-type-tag-class-face ((t (:weight bold))))
   '(ecb-type-tag-enum-face ((t (:weight bold))))
   '(ecb-type-tag-group-face ((t (:foreground "dim gray" :weight bold))))
   '(ecb-type-tag-interface-face ((t (:weight bold))))
   '(ecb-type-tag-struct-face ((t (:weight bold))))
   '(ecb-type-tag-typedef-face ((t (:weight bold))))
   '(ecb-type-tag-union-face ((t (:weight bold)))))
  )


(eval-after-load "ecb"
  '(ecb-face-settings))


(provide 'ecb-face-settings)
