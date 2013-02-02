;; -*- Emacs-Lisp -*-
;; Face settings for `semantic'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 13:26>

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


(require 'semantic)


(defun semantic-face-settings ()
  "Face settings for `semantic'."
  (custom-set-faces
   '(semantic-decoration-on-includes ((t (nil))))
   '(semantic-decoration-on-private-members-face ((t (:background "#200000"))))
   '(semantic-decoration-on-protected-members-face
      ((t (:background "#000020"))))
   '(semantic-decoration-on-unknown-includes ((t (:background "#900000"))))
   '(semantic-decoration-on-unparsed-includes ((t (:background "#555500"))))
   '(semantic-highlight-edits-face
      ((((class color) (min-colors 88)) (:background "gray20"))
       (((class color) (min-colors 16)) (:background "gray20"))
       (((class color) (min-colors 8)) (nil))
       (((type tty) (class mono)) (nil))
       (t (:background "gray20"))))
   '(semantic-highlight-func-current-tag-face
      ((((class color) (min-colors 88)) (:background "gray20"))
       (((class color) (min-colors 16)) (:background "gray20"))
       (((class color) (min-colors 8)) (:inverse-video t))
       (((type tty) (class mono)) (:inverse-video t))
       (t (:background "gray20"))))
   '(semantic-tag-boundary-face ((t (:overline "cyan"))))
   '(semantic-unmatched-syntax-face ((t (nil)))))
  )


(eval-after-load "semantic"
  '(semantic-face-settings))


(provide 'semantic-face-settings)
