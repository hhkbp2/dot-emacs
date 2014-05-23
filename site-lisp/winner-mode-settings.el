;; -*- Emacs-Lisp -*-
;; Settings for `winner-mode-settings'.

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-05-23 16:54>

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


(require 'winner)


(defun winner-mode-settings ()
  "Settings for `winner-mode'."

  ;; disable default winner-mode keybindings
  (setq winner-dont-bind-my-keys t)
  (winner-mode t)
  ;; bind `winner-undo' to `Ctrl-x 4'
  (define-key ctl-x-map "4" 'winner-undo)
  ;; bind `winner-redo' to `Ctrl-x 5'
  (define-key ctl-x-map "5" 'winner-redo)
  )

(eval-after-load "winner"
  `(winner-mode-settings))


(provide 'winner-mode-settings)
