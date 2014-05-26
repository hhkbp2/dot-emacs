;; -*- Emacs-Lisp -*-
;; Settings for `flycheck'.

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-05-26 17:43>

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
                  flycheck-4-go))
    (funcall func))
  )


(eval-after-load "flycheck"
  `(flycheck-settings))


(provide 'flycheck-settings)
