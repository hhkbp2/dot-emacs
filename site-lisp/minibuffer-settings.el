;;; minibuffer-settings.el --- Settings for `minibuffer'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:39>

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
;;
;; `minibuffer'
;; 状态栏下面的辅助输入区

;;; Code:


(defun minibuffer-settings ()
  "Settings for `minibuffer'."

  ;; 默认启用minibuffer
  (minibuffer-electric-default-mode t)

  ;; 允许minibuffer自由变化宽度
  (setq resize-mini-windows t)

  ;; recursive commands in minibuffer
  (setq enable-recursive-minibuffers t)

  ;; 在minibuffer里启用icomplete mode，自动补全函数和变量
  (icomplete-mode t)

  ;; 启用部分补全功能，如输入M-x q r r相当于M-x query-replace-regexp
  (if (>= emacs-major-version 24)
      (progn
    (setq completion-styles '(partial-completion initials))
    (setq completion-pcm-complete-word-inserts-delimiters t))
    (partial-completion-mode t))

  ;; `completion-ignored-extensions': 字符串列表，常为后缀
  ;; 补全文件名时不把以其中字符串结尾的文件名列为侯选
  (dolist (file-postfix
           '(;; tarball
             ".bz2" ".7z" ".zip" ".gz" ".tar"
             ;; specified doc
             ".ps" ".pdf" ".chm" ".doc" ".deb"
             ;; picture
             ".png" ".jpg" ".JPG" ".bmp" ".BMP"
             ;; font
             ".ttf"
             ;; directory
             ".hg/" ".svn/" ".git"
             ;; compiled object and temporary file
             ".elc" ; elisp
             ".pyc" ; python
             ".beam" ".boot" ; erlang
             ))
    (add-to-list 'completion-ignored-extensions file-postfix))

  ;; 当寻找一个同名的文件，自动关联上那个文件
  (setq uniquify-buffer-name-style 'forward))


(eval-after-load "minibuffer"
  `(minibuffer-settings))


(provide 'minibuffer-settings)

;;; minibuffer-settings.el ends here
