;;; awk-mode-settings.el --- Settings for `awk-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:08>

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


(require 'cc-mode)


;; 定制`awk-mode'缩进风格
(defconst dw-awk-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     (defun-open after)
     (defun-close . c-snug-1line-defun-close)
     (substatement-open after)
     (block-close . c-snug-do-while)
     (arglist-cont-nonempty))
    (c-hanging-semi&comma-criteria)
    (c-cleanup-list)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open . 0)
     (statement-cont . +)))
  "Dylan.Wen's awk indentation style based on built-in style `awk'.")


(defun awk-mode-settings ()
  "Settings for `awk-mode'."
  (c-add-style "dw-awk-style" dw-awk-style)
  (c-set-style "dw-awk-style"))


(add-hook 'awk-mode-hook
          'awk-mode-settings)


(provide 'awk-mode-settings)

;;; awk-mode-settings.el ends here
