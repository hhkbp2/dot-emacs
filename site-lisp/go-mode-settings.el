;; -*- Emacs-Lisp -*-
;; Settings for `go-mode'.

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-05-30 17:27>

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


(require 'gocode-settings)
(require 'golint)


(defun go-remove-unused-imports-before-save ()
  "Add this to .emacs to run `go-remove-unused-imports' on buffer saving:
 (add-hook 'before-save-hook 'go-remove-unused-imports)."

  (interactive)
  (when (eq major-mode 'go-mode)
    (go-remove-unused-imports nil)))


(defun go-mode-settings ()
  "Settings for `go-mode'."

  ;; run gofmt on the current buffer when saving
  ;; non `go-mode' buffer would be intact
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; TODO not working now, fix it
  ;; run `go-remove-unused-imports' on the current buffer when saving
  ;;(add-hook 'before-save-hook 'go-remove-unused-imports-before-save)

  )

(eval-after-load "go-mode"
  `(go-mode-settings))


(provide 'go-mode-settings)
