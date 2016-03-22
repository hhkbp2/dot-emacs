;;; smartparens-settings.el --- Settings for `smartparens'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:59>

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


(require 'smartparens-config)
(require 'smartparens-face-settings)


(defun smartparens-settings()
  "Settings for `smartparens'."
  (smartparens-face-settings)
  (show-smartparens-face-settings)
  )

(eval-after-load "smartparens"
  `(smartparens-settings))

(smartparens-global-mode t)
(show-smartparens-global-mode t)

(provide 'smartparens-settings)

;;; smartparens-settings.el ends here
