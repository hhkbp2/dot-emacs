;; -*- Emacs-Lisp -*-
;; Settings for `scheme-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-12-14 18:17>

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


;;(require 'xscheme)
(require 'scheme)
(if (dw-version->=-23.3) (require 'geiser-install))


(defun scheme-mode-settings ()
  "Settings for `scheme-mode'."

  ;;(make-local-variable 'scheme-indent-function)
  )

(eval-after-load "scheme-mode"
  `(scheme-mode-settings))


(provide 'scheme-mode-settings)
