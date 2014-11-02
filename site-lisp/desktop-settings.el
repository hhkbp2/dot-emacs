;; -*- Emacs-Lisp -*-
;; Settings for `desktop'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-11-02 17:32>

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
;; `desktop'
;; keep records of workspace(desktop) in emacs

;;; Code:


(require 'desktop)


(defun desktop-settings ()
  "Settings for `desktop'."

  (let ((desktop-file-path "~/.emacs.d/desktop"))
    ;; make sure it exists
    (make-directory desktop-file-path 'NO-ERROR)
    (add-to-list 'desktop-path desktop-file-path))

  (setq desktop-base-file-name "emacs.desktop")
  (desktop-load-default)
  (desktop-save-mode 1)
  )


(eval-after-load "desktop"
  `(desktop-settings))


(provide 'desktop-settings)
