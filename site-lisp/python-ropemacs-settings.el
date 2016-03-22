;;; python-ropemacs-settings.el --- Settings for `python-ropemacs'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:50>

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


(require' pymacs-settings)


(defun python-ropemacs-settings ()
  "Settings for `python-ropemacs'."

  ;; TODO what to set?
  )


(defun python-ropemacs-load()
  "Load ropemacs library."

;;  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-global-prefix "C-c p")

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os" "sys"))
  (ropemacs-mode t))


(provide 'python-ropemacs-settings)

;;; python-ropemacs-settings.el ends here
