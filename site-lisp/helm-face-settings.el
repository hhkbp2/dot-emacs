;; -*- Emacs-Lisp -*-
;; Face settings for `helm'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-04 04:55>

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


(defun helm-face-settings ()
  "Face settings for `helm'."
  (custom-set-faces
   '(helm-source-header
     ((((type x)) (:foreground "#bb66ff" :weight bold))
      (((class color) (min-colors 256)) (:foreground "#bb66ff" :weight bold))
      (((class color) (min-colors 16)) (:foreground "purple" :weight bold))
      (((class color) (min-colors 8)) (:foreground "purple" :weight bold))
      (((type tty) (class mono)) (:foreground "purple" :weight bold))
      (t (:foreground "#bb66ff" :weight bold))))
   '(helm-selection
     ((((type x)) (:background "black"))
      (((class color) (min-colors 256)) (:background "black"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (((type tty) (class mono)) (:background "black"))
      (t (:background "black"))))
   )
  )

(defun helm-files-face-settings ()
  "Face settings for `helm-files'."

  ;; TODO add impl
  )


(eval-after-load "helm"
  `(helm-face-settings))

(eval-after-load "helm-files"
  `(helm-files-face-settings))


(provide 'helm-face-settings)
