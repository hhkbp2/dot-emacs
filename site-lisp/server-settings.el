;; -*- Emacs-Lisp -*-
;; Settings for `server'.

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-08-28 11:57>

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


(require 'server)


(defun server-settings ()
  "Settings for `server'."

  (setq server-socket-dir "~/.emacs.d/server")
  )


(eval-after-load "server"
  `(server-settings))


(provide 'server-settings)
