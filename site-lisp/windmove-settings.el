;;; windmove-settings.el --- Settings for `windmove'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2014 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 15:19>

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


(require 'windmove)


(defun windmove-settings ()
  "Settings for `windmove'."

  ;; 把默认的windows move键modifier由`shift'换成`meta'
  ;; 用 `modifier-{left,right,up,down}'可以在打开的窗口中的跳转
  (windmove-default-keybindings 'meta)
  )

(eval-after-load "windmove"
  `(windmove-settings))


(provide 'windmove-settings)

;;; windmove-settings.el ends here
