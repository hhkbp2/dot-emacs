;;; go-mode-settings.el --- Settings for `go-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2017-04-15 23:59>

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

(defun dw-go-tidb-gopath ()
  "Detect GOPATH for the tidb project."
  (seq-some #'dw-go--tidb-gopath ["_vendor" "vendor"]))

(defun dw-go--tidb-gopath (filename)
  "Detect GOPATH using the hint FILENAME."
  (let* ((d (locate-dominating-file buffer-file-name "tidb"))
         (tidb-root (concat d (file-name-as-directory "tidb")))
         (vendor (concat tidb-root (file-name-as-directory filename))))
    (if (and d
             (file-exists-p vendor))
        (cons vendor (go-plain-gopath)))))

(defadvice gofmt (before gofmt-prepare-gopath activate)
  "Detect GOPATH and set it before running `gofmt'."
  (go-set-project))

(defadvice gofmt (after gofmt-reset-gopath activate)
  "Reset GOPATH to the value it had before running `gofmt'."
  (go-reset-gopath))

(defun go-mode-settings ()
  "Settings for `go-mode'."

  ;; run goimports before saving golang buffer
  ;; non `go-mode' buffer would be intact
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; add the gopath guessing function for tidb
  (add-to-list 'go-guess-gopath-functions #'dw-go-tidb-gopath)

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
               (local-set-key (kbd "M-.") 'godef-jump)))
  )

(eval-after-load "go-mode"
  `(go-mode-settings))


(provide 'go-mode-settings)

;;; go-mode-settings.el ends here
