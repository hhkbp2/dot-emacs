;; -*- Emacs-Lisp -*-
;; Settings for `c-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-01-23 13:53>

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


;; 定制`c-mode'缩进风格
(defconst dw-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist . ((statement-block-intro . +)
                        (knr-argdecl-intro . 0)
                        (substatement-open . 0)
                        (substatement-label . 0)
                        (label . 0)
                        (statement-cont . +)
                        (topmost-intro . 0)
                        (case-label . +)
                        (statement-case-intro . +))))
  "Dylan.Wen's c indentation style based on built-in style `k&r'.")


(defun c-mode-settings ()
  "Settings for `c-mode'."
  (c-add-style "dw-c-style" dw-c-style)
  (c-set-style "dw-c-style"))


(add-hook 'c-mode-hook
          'c-mode-settings)


(provide 'c-mode-settings)
