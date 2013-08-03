;; -*- Emacs-Lisp -*-
;; Settings for `smex'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-04 03:58>

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


(require 'smex)


(defun smex-settings ()
  "Settings for `smex'."

  ;; autoloaded
  ;;(smex-initialize)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'smex-update)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )

(eval-after-load "smex"
  `(smex-settings))


(provide 'smex-settings)
