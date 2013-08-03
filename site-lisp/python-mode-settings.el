;; -*- Emacs-Lisp -*-
;; Settings for `python-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-03 14:56>

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


(require 'python)
(require 'whitespace)
(require 'pymacs-settings)
(require 'python-ropemacs-settings)
(require 'pycomplete-settings)
(require 'jedi-settings)


(defun python-mode-settings ()
  "Settings for `python-mode'."

  ;; set tab width
  (setq tab-width 4)

  ;; show whitespace/tab
  (setq whitespace-style
        '(face indentation::tab indentation::space tabs tab-mark trailing))
  (whitespace-mode 1)

  ;; load ropemacs settings
  (python-ropemacs-settings)

  ;; key binding settings
  ;; backspace on a tty
  (define-key python-mode-map "\177" 'c-hungry-backspace)
  ;; backspace on gui
  (define-key python-mode-map [backspace] 'c-hungry-backspace)
  ;; delete on a tty
  (define-key python-mode-map [deletechar] 'c-hungry-delete-forward)
  ;; delete on a gui
  (define-key python-mode-map [delete] 'c-hungry-delete-forward)
  ;; delete on point
  (define-key python-mode-map [(control d)] 'c-hungry-delete-forward)

  (define-key python-mode-map [(control c) (c)] 'comment-dwim)
  (define-key python-mode-map [(control c) (control c)] 'comment-dwim)
  )


;; load python mode settings everytime loading python mode
;; (in general, that is when a python file is open)
(add-hook 'python-mode-hook
          'python-mode-settings)


;; load ropemacs only when first time load file `python.el'
(eval-after-load "python"
  `(python-ropemacs-load))


(provide 'python-mode-settings)
