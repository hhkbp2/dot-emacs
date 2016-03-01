;; -*- Emacs-Lisp -*-
;; Settings for `flycheck'.

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-01 14:28>

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


(defun flycheck-4-go ()
  (require 'go-flycheck)
  (add-hook 'go-mode-hook 'flycheck-mode))

(defun flycheck-4-elisp ()
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(defun flycheck-4-cc-mode ()
  ;; TODO add impl
  )

(defun flycheck-4-python ()
  (add-hook 'python-mode-hook '(lambda ()
                                 (flycheck-mode)
                                 (flycheck-select-checker 'python-pylint))))

(defun flycheck-4-rust ()
  (require 'flycheck-rust)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(defun flycheck-settings ()
  "Settings for `flycheck'."

  ;; check syntax when
  ;; 1. `flycheck-mode' is enabled
  ;; 2. the buffer is save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-completion-system 'ido)

  ;; enable flycheck in specified modes
  (dolist (func '(flycheck-4-elisp
                  flycheck-4-cc-mode
                  flycheck-4-python
                  flycheck-4-rust
                  flycheck-4-go))
    (funcall func))
  )


(eval-after-load "flycheck"
  `(flycheck-settings))


(provide 'flycheck-settings)
