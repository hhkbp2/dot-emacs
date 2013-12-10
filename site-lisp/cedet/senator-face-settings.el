;; -*- Emacs-Lisp -*-
;; Face settings for `senator'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 13:42>

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


(defun senator-face-settings ()
  "Face settings for `senator'."
  (custom-set-faces
   '(senator-intangible-face ((t (:foreground "gray75"))))
   '(senator-momentary-highlight-face ((t (:background "gray30"))))
   '(senator-read-only-face ((t (:background "#664444")))))
  )


(eval-after-load "senator"
  '(senator-face-settings))


(provide 'senator-face-settings)
