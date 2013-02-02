;; -*- Emacs-Lisp -*-
;; Settings for `abbrev-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-01-21 13:09>

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


(defun abbrev-settings ()
  "Settings for `abbrev-mode'."

  ;; 默认打开缩写模式
  (setq-default abbrev-mode t)
  ;; 设置缩写定义文件
  (read-abbrev-file "~/.emacs.d/abbrev_defs")
  ;; 设置自动保存缩写定义文件
  (setq save-abbrevs t))


(eval-after-load "abbrev-mode"
  `(abbrev-settings))


(provide 'abbrev-settings)
