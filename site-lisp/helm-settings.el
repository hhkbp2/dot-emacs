;; -*- Emacs-Lisp -*-
;; Settings for `helm'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-04 04:56>

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


(require 'helm)
(require 'helm-match-plugin)
(require 'helm-config)
(require 'helm-face-settings)


(defun helm-settings ()
  "Settings for `helm'."

  ;; use helm for all
  (helm-mode 1)

  (global-set-key [(control x) (control f)] 'helm-find-files)
  )


(eval-after-load "helm"
  `(helm-settings))


(provide 'helm-settings)
