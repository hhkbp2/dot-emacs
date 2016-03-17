;; -*- Emacs-Lisp -*-
;; Settings for `guide-key'.

;; Copyright (C) 2016 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-17 12:29>

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


(require 'guide-key)


(defun guide-key-settings ()
  "Settings for `guide-key-mode'."

  ;; (setq guide-key/guide-key-sequence '("C-x"))
  ;; (setq guide-key/recursive-key-sequence-flag t)
  )

(eval-after-load "guide-key"
  `(guide-key-settings))

;; Enable guide-key-mode
;;(guide-key-mode 1)


(provide 'guide-key-settings)
