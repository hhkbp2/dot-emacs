;; -*- Emacs-Lisp -*-
;; Settings for `jedi'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-03 14:45>

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


(require 'jedi-autoloads)


(defun jedi-settings ()
  "Settings for `jedi'."

  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  )

(jedi-settings)


(provide 'jedi-settings)
