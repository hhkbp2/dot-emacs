;;; saveplace-settings.el --- Settings for `saveplace'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:53>

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
;; `saveplace'
;; save where cursor is when exit

;;; Code:


(require 'saveplace)


(defun saveplace-settings ()
  "Settings for `saveplace'."

  (setq-default save-place t)

  (setq save-place-file "~/.emacs.d/emacs-places")
  )

(eval-after-load "saveplace"
  `(saveplace-settings))


(provide 'saveplace-settings)

;;; saveplace-settings.el ends here
