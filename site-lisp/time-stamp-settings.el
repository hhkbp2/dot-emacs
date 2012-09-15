;; -*- Emacs-Lisp -*-
;; Settings for `time-stamp'

;; Copyright (C) 2009, 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-01-21 13:17>

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
;; `time-stamp'
;; keep time stamp when saving files

;;; Code:


(add-hook 'before-save-hook 'time-stamp)


(defun time-stamp-settings ()
  "Settings for `time-stamp'."

  ;; set time-stamp format
  (setq time-stamp-format "%04y-%02m-%02d %02H:%02M")
  )

(eval-after-load "time-stamp"
  `(time-stamp-settings))


(provide 'time-stamp-settings)
