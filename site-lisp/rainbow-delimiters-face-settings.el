;; -*- Emacs-Lisp -*-
;; Face settings for `rainbow-delimiters'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2013-01-06 12:10>

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



(defun rainbow-delimiters-face-settings ()
  "Face settings for `rainbow-delimiters'."

  (custom-set-faces
   '(rainbow-delimiters-depth-1-face
     ((((type x)) (:foreground "burlywood"))
      (((class color) (min-colors 256)) (:foreground "burlywood"))
      (((class color) (min-colors 16)) (:foreground "burlywood"))
      (((class color) (min-colors 8)) (:foreground "burlywood"))
      (t (:foreground "burlywood"))))
   ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
   ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
   ;; '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
   )
  )

(eval-after-load "rainbow-delimiters"
  `(rainbow-delimiters-face-settings))


(provide 'rainbow-delimiters-face-settings)
