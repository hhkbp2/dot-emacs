;; -*- Emacs-Lisp -*-
;; Face settings for `cal-china-x'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 14:01>

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


(defun cal-china-x-face-settings ()
  "Face settings for `cal-china-x'."
  (custom-set-faces
   '(cal-china-x-general-holiday-face ((t (:background "green"))))
   '(cal-china-x-important-holiday-face ((t (:background "red")))))
  )


(eval-after-load "cal-china-x"
  '(cal-china-x-face-settings))


(provide 'cal-china-x-face-settings)
