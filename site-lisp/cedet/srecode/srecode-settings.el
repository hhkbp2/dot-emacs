;; -*- Emacs-Lisp -*-
;; Settings for `srecode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-12-10 21:38>

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


(require 'srecode)
(require 'srecode-face-settings)


;; set location of srecode map file
(setq-default srecode-map-save-file "~/.emacs.d/srecode/srecode-map")
;; set location of srecode templates
;;(add-to-list 'srecode-map-load-path "~/.emacs.d/srecode")

;; enable srecode (template management) minor-mode.
(global-srecode-minor-mode 1)


(defun srecode-settings ()
  "Settings for `srecode'.")

(eval-after-load "srecode"
  `(srecode-settings))


(provide 'srecode-settings)
