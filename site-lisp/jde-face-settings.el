;; -*- Emacs-Lisp -*-
;; Face settings for `jde'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-04-08 22:08>

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


(defun jde-face-settings ()
  "Face settings for `jde'."
  (custom-set-faces
   '(jde-java-font-lock-bold-face
     ((t (:weight bold))))
   '(jde-java-font-lock-italic-face
     ((t (:slant italic))))
   '(jde-java-font-lock-underline-face
     ((t (:underline t))))
   '(jde-java-font-lock-api-face
     ((t :inherit font-lock-function-name-face)))
   '(jde-java-font-lock-code-face
     ((t nil)))
   '(jde-java-font-lock-constant-face
     ((t :inherit font-lock-constant-face)))
   ;; '(jde-java-font-lock-constructor-face
   ;;   )
   '(jde-java-font-lock-javadoc-face
     ((((class color) (min-colors 88))
       (:foreground "#bb66ff" :slant italic))
      (((class color) (min-colors 16))
       (:foreground "purple" :slant italic))
      (((class color) (min-colors 8))
       (:foreground "purple" :slant italic))
      (((type tty) (class mono))
       (:foreground "purple" :slant italic))
      (t (:foreground "#bb66ff" :slant italic))))
   '(jde-java-font-lock-doc-tag-face
     ((t :inherit font-lock-comment-face)))
   '(jde-java-font-lock-link-face
     ((((class color) (min-colors 88))
       (:foreground "#bb66ff" :slant italic :underline t))
      (((class color) (min-colors 16))
       (:foreground "purple" :slant italic :underline t))
      (((class color) (min-colors 8))
       (:foreground "purple" :slant italic :underline t))
      (((type tty) (class mono))
       (:foreground "purple" :slant italic :underline t))
      (t (:foreground "#bb66ff" :slant italic))))
   '(jde-java-font-lock-modifier-face
     ((t :inherit font-lock-keyword-face)))
   '(jde-java-font-lock-number-face
     ((t :inherit font-lock-constant-face)))
   '(jde-java-font-lock-operator-face
     ((t :inherit font-lock-builtin-face)))
   '(jde-java-font-lock-package-face
     ((t :inherit font-lock-preprocessor-face)))
   '(jde-java-font-lock-pre-face
     ((((class color) (min-colors 88))
       (:foreground "#800080" :slant italic))
      (((class color) (min-colors 16))
       (:foreground "purple" :slant italic))
      (((class color) (min-colors 8))
       (:foreground "purple" :slant italic))
      (((type tty) (class mono))
       (:foreground "purple" :slant italic))
      (t (:foreground "#800080" :slant italic))))
   '(jde-java-font-lock-private-face
     ((t :inherit font-lock-keyword-face)))
   '(jde-java-font-lock-protected-face
     ((t :inherit font-lock-keyword-face)))
   '(jde-java-font-lock-public-face
     ((t :inherit font-lock-keyword-face)))))


(eval-after-load "jde"
  '(jde-face-settings))


(provide 'jde-face-settings)

