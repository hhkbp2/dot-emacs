;; -*- Emacs-Lisp -*-
;; Settings for `common-lisp-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-10-25 19:39>

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


(require 'slime-settings)
(require 'dw-functionals)


(defun common-lisp-mode-settings ()
  "Settings for `common-lisp-mode'."
  )


(slime-settings)

(dw-add-file-mode-pattern-list '(("\\.lisp$" . common-lisp-mode)
                                 ("\\.cl$" . common-lisp-mode)))


(provide 'common-lisp-mode-settings)
