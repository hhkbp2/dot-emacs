;;; go-mode-settings.el --- Settings for `go-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:45>

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


(require 'golint)
(require 'dw-functionals)
(require 'go-mode)


(defun go-mode-settings ()
  "Settings for `go-mode'."

  ;; run gofmt on the current buffer when saving
  ;; non `go-mode' buffer would be intact
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; key bindings
  (dw-hungry-delete-on-mode-map go-mode-map)
  (dw-commet-dwin-on-mode-map go-mode-map)

  ;; Enable `subword-mode' since go is Camel style.
  (add-hook 'go-mode-hook
            '(lambda ()
               ;; turn on `go-eldoc'
               (go-eldoc-setup)

               ;; turn on `subword-mode' since golang uses camel case.
               (subword-mode)
               ;; keybindings
               (local-set-key (kbd "M-.") 'godef-jump)
               (local-set-key (kbd "C-c i") 'go-goto-imports)
               (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
               ))
  )

(eval-after-load "go-mode"
  `(go-mode-settings))


(provide 'go-mode-settings)

;;; go-mode-settings.el ends here
