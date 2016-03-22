;;; python-mode-settings.el --- Settings for `python-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:49>

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


(require 'whitespace)
(require 'python-base-settings)
(require 'pymacs-settings)
(require 'python-ropemacs-settings)
(require 'jedi-settings)
(require 'dw-functionals)
(require 'python-mode)


(defun dw-load-pylint-and-pep8 ()

  ;; This line is needed to fix the bug:
  ;; "Symbol's function definition is void: tramp-tramp-file-p"
  ;; when `python-pylint' or `python-pep8' starts up.
  (require 'tramp)

  (require 'python-pylint-autoloads)
  (require 'python-pep8-autoloads)
  )
(dw-load-pylint-and-pep8)


(defun python-pylint-settings()
  "Settings for `python-pylint'."

  )

(defun python-pep8-settings ()
  "Settings for `python-pep8'."
  )


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

  ;; key bindings
  (dw-hungry-delete-on-mode-map python-mode-map)
  (dw-commet-dwin-on-mode-map python-mode-map)

  (define-key python-mode-map (kbd "RET") 'py-newline-and-indent)
  )


;; load python mode settings everytime loading python mode
;; (in general, that is when a python file is open)
(eval-after-load "python-mode"
  '(python-mode-settings))


;; load ropemacs only when first time load file `python.el'
(eval-after-load "python-mode"
  `(python-ropemacs-load))


(provide 'python-mode-settings)

;;; python-mode-settings.el ends here
