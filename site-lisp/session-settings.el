;;; session-settings.el --- Settings for `session'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:56>

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
;; `session'
;; keep records of session in emacs

;;; Code:


(autoload 'session-initialize "session"
"Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'." t)


(add-hook 'after-init-hook 'session-initialize)


(defun session-settings ()
  "Settings for `session'."

  (setq session-save-file "~/.emacs.d/session"
        session-initialize '(session places menus))
  )

(eval-after-load "session"
  `(session-settings))


(provide 'session-settings)

;;; session-settings.el ends here
