;; -*- Emacs-Lisp -*-
;; Settings for `whitespace'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2013-01-01 19:53>

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


(defun whitespace-settings ()
  "Settings for `whitespace'."

  ;; refer to http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [8629 10]) ; 10 LINE FEED, 8629 DOWNWARDS ARROW WITH CORNER LEFTWARDS 「↵」
          (tab-mark 9 [8677 9] [92 9]) ; 9 TAB, 8677 RIGHTWARDS ARROW TO BAR 「⇥」
          ))
  )

(eval-after-load "whitespace"
  `(whitespace-settings))


(provide 'whitespace-settings)
