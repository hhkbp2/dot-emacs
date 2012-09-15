;; -*- Emacs-Lisp -*-
;; Settings for `cal-china-x'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 13:56>

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
;;
;; `cal-china-x':
;; calculating chinese lunar holiday

;;; Code:


(require 'cal-china-x)
;; load face settings
(require 'cal-china-x-face-settings)


(defun cal-china-x-settings ()
  "Settings for `cal-china-x'."
  )


(eval-after-load "cal-china-x"
  '(cal-china-x-settings))


(provide 'cal-china-x-settings)