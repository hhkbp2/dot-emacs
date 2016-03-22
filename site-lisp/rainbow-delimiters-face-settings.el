;;; rainbow-delimiters-face-settings.el --- Face settings for `rainbow-delimiters'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:51>

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
   '(rainbow-delimiters-unmatched-face
     ((((type x)) (:background "red" :weight bold :inverse-video t))
      (((class color) (min-colors 256)) (:background "red" :weight bold :inverse-video t))
      (((class color) (min-colors 16)) (:background "red" :weight bold :inverse-video t))
      (((class color) (min-colors 8)) (:background "red" :weight bold :inverse-video t))
      (t (:background "red" :weight bold :inverse-video t))))
   '(rainbow-delimiters-depth-1-face
     ((((type x)) (:foreground "gray60"))
      (((class color) (min-colors 256)) (:foreground "gray60"))
      (((class color) (min-colors 16)) (:foreground "gray60"))
      (((class color) (min-colors 8)) (:foreground "gray60"))
      (t (:foreground "gray60"))))
   '(rainbow-delimiters-depth-2-face
     ((((type x)) (:foreground "orchid"))
      (((class color) (min-colors 256)) (:foreground "orchid"))
      (((class color) (min-colors 16)) (:foreground "orchid"))
      (((class color) (min-colors 8)) (:foreground "orchid"))
      (t (:foreground "orchid"))))
   '(rainbow-delimiters-depth-3-face
     ((((type x)) (:foreground "dodger blue"))
      (((class color) (min-colors 256)) (:foreground "dodger blue"))
      (((class color) (min-colors 16)) (:foreground "dodger blue"))
      (((class color) (min-colors 8)) (:foreground "dodger blue"))
      (t (:foreground "dodger blue"))))
   '(rainbow-delimiters-depth-4-face
     ((((type x)) (:foreground "sky blue"))
      (((class color) (min-colors 256)) (:foreground "sky blue"))
      (((class color) (min-colors 16)) (:foreground "sky blue"))
      (((class color) (min-colors 8)) (:foreground "sky blue"))
      (t (:foreground "sky blue"))))
   '(rainbow-delimiters-depth-5-face
     ((((type x)) (:foreground "lime green"))
      (((class color) (min-colors 256)) (:foreground "lime green"))
      (((class color) (min-colors 16)) (:foreground "lime green"))
      (((class color) (min-colors 8)) (:foreground "lime green"))
      (t (:foreground "lime green"))))
   '(rainbow-delimiters-depth-6-face
     ((((type x)) (:foreground "lawn green"))
      (((class color) (min-colors 256)) (:foreground "lawn green"))
      (((class color) (min-colors 16)) (:foreground "lawn green"))
      (((class color) (min-colors 8)) (:foreground "lawn green"))
      (t (:foreground "lawn green"))))
   '(rainbow-delimiters-depth-7-face
     ((((type x)) (:foreground "gold"))
      (((class color) (min-colors 256)) (:foreground "gold"))
      (((class color) (min-colors 16)) (:foreground "gold"))
      (((class color) (min-colors 8)) (:foreground "gold"))
      (t (:foreground "gold"))))
   '(rainbow-delimiters-depth-8-face
     ((((type x)) (:foreground "sandy brown"))
      (((class color) (min-colors 256)) (:foreground "sandy brown"))
      (((class color) (min-colors 16)) (:foreground "sandy brown"))
      (((class color) (min-colors 8)) (:foreground "sandy brown"))
      (t (:foreground "sandy brown"))))
   '(rainbow-delimiters-depth-9-face
     ((((type x)) (:foreground "tomato"))
      (((class color) (min-colors 256)) (:foreground "tomato"))
      (((class color) (min-colors 16)) (:foreground "tomato"))
      (((class color) (min-colors 8)) (:foreground "tomato"))
      (t (:foreground "tomato"))))))

(eval-after-load "rainbow-delimiters"
  `(rainbow-delimiters-face-settings))


(provide 'rainbow-delimiters-face-settings)

;;; rainbow-delimiters-face-settings.el ends here
