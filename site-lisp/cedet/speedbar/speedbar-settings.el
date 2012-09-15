;; -*- Emacs-Lisp -*-
;; Settings for `speedbar'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 13:09>

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


(require 'speedbar-loaddefs)
(require 'speedbar-face-settings)

(defun speedbar-settings ()
  "Settings for `speedbar'."

  ;; display all files (can't expand ones) in speedbar buffer
  (setq speedbar-show-unknown-files t)
  ;; key bindings
  (global-set-key [(control c) (s) (s)] 'speedbar)

  (define-key speedbar-key-map (kbd "<up>") 'speedbar-prev)
  (define-key speedbar-key-map (kbd "<down>") 'speedbar-next)
  (define-key speedbar-key-map (kbd "<left>") 'speedbar-contract-line)
  (define-key speedbar-key-map (kbd "<right>") 'speedbar-expand-line)
  )


(eval-after-load "speedbar"
  `(speedbar-settings))


(provide 'speedbar-settings)