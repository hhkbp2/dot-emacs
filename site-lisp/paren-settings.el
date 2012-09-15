;; -*- Emacs-Lisp -*-
;; Settings for `paren'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 13:19>

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


(require 'paren)
;; load face settings
(require 'paren-face-settings)


(defun paren-settings ()
  "Settings for `paren'."
  ;; highlight matching(unmatching)parenthese around cursor
  (show-paren-mode t)
  ;; parenthese style, 高亮匹配的括号
  (setq show-paren-style 'parentheses)
  ;; parenthese style, 高亮匹配的括号及括号中内容
  ;;(setq show-paren-style 'expression)
  )


(eval-after-load "paren"
  '(paren-settings))


(provide 'paren-settings)