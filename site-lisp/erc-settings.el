;; -*- Emacs-Lisp -*-
;; Settings for `erc'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-09-15 00:53>

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


(require 'erc)
(require 'erc-face-settings)
(require 'dw-functionals)


(defun xwl-erc-format-nick (&optional user channel-data)
  "Like `erc-format-nick' but trim nick to a fixed length."
  (let ((nick (erc-format-nick user channel-data)))
    (when (> (length nick) 9)
      (setq nick (concat (substring nick 0 6)
                         ".."
                         (substring nick -1))))
    nick))


(defvar xwl-erc-last-datestamp nil)
(make-variable-buffer-local 'xwl-erc-last-datestamp)

(defadvice erc-insert-timestamp-left (around insert-datestamp activate)
  ad-do-it
  (let ((datestamp (erc-format-timestamp (current-time)
                                         xwl-erc-datestamp-format)))
    (unless (string= datestamp xwl-erc-last-datestamp)
      (ad-set-arg 0 datestamp)
      ad-do-it
      (setq xwl-erc-last-datestamp datestamp))))


(defun erc-settings ()
  "Settings for `erc'."

  ;; enable a menu for erc
  (require 'easymenu)
  (easy-menu-add-item  nil '("tools") ["IRC with ERC" erc t])

  (setq
   ;; don't change hostname on display
   erc-common-server-suffixes nil
   ;; set display format in mode line
   erc-mode-line-format "%t %a"
   ;; 设置join打开频道时新建buffer在后台打开
   erc-join-buffer 'bury
   ;; 设置收到私人消息时新建buffer在后台打开
   erc-auto-query 'bury)

  ;; timestamp
  (erc-timestamp-mode 1)

  (setq
   ;; 只在时间改变时才更新插入timestamp
   erc-timestamp-only-if-changed-flag t
   ;; 控制时间戳的显示位置在左边
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   ;; 设置timestamp的格式
   erc-timestamp-format "%H:%M ")

  (setq xwl-erc-datestamp-format " ========== [%H:%M %b %d %A] ==========\n")


  ;;设定聊天时的行宽，方便阅读
  (setq erc-fill-column 80)

  ;; 设置滚动，保持输入在当前窗口的最后一行
  ;;(add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)


  ;; set charactor encoding and decoding
  (setq
   ;; set default coding for incoming and outgoing text
   erc-default-coding-system '(utf-8 . utf-8)
   ;; create a query buffer each time you receive a private message
   erc-auto-query t)

  ;; turn on auto join
  (erc-autojoin-mode 1)

  (setq erc-ignore-list nil)

  ;; 设置要隐藏的IRC消息的类型
  (setq erc-hide-list
        '(
          ;;        "353"     ;; 忽略昵称列表
          "JOIN"    ;; 加入
          "PART"    ;; 离开
          "QUIT"    ;; 退出
          "MODE"    ;; 改变模式
          ))

  ;; message match and highlight
  (erc-match-mode 1)
  (setq erc-current-nick-highlight-type 'nick-or-keyword
        erc-keywords nil
        erc-pals nil)

  ;; fill and format
  (erc-fill-mode 1)
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 12
        erc-fill-prefix nil)

  ;; trim erc nicks
  (setq erc-format-nick-function 'xwl-erc-format-nick)

  ;; set prompt as channel-specific
  (setq erc-prompt
        (lambda ()
          (erc-propertize
           (concat
            (if (and (boundp 'erc-default-recipients)
                     (erc-default-target))
                (erc-default-target)
              "ERC")
            ">")
           'read-only t
           'rear-nonsticky t
           'front-nonsticky t)))

  (custom-set-variables
   '(erc-modules
     (quote
      (autojoin
       button completion fill irccontrols list match menu move-to-prompt
       netsplit networks noncommands readonly ring services stamp spelling
       track))))

  ;; highlight nick name
  (require 'erc-highlight-nicknames)
  (add-to-list 'erc-modules 'highlight-nicknames)

  (dw-load-related-file-if-exist "../personal/dw-erc-settings.el")
  )


(eval-after-load "erc"
  '(erc-settings))


(provide 'erc-settings)
