;; -*- Emacs-Lisp -*-
;; Settings for software development.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2013-01-06 11:44>

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


(require 'dev-base)
(require 'dw-functionals)


;; 高亮光标处单词
(require 'highlight-symbol-settings)


;; 高亮最新做的修改
;;(require 'highlight-tail-settings)


;; 高亮当前行
(require 'highlight-current-line-settings)


;; company, 自动补全
;;(require 'company-settings)


;; hs-minor-mode,折叠代码
(require 'hs-minor-mode-settings)

(require 'whitespace-settings)

;;; 配置右括号自动补全
(require 'skeleton-settings)


;;; 去tab化
(defun my-untabify()
  "Replace TAB with whitespace."
  (add-hook 'local-write-file-hooks
            '(lambda()
               (save-excursion
                 (untabify (point-min) (point-max))))))


;;; 删除行末空白
(defun my-delete-trailing-space()
  "Delete the trailing whitespace."
  (add-hook
   'write-contents-functions ;; the buffer local hook
   ;; 'write-file-functions  ;; alternative, the global hook
   'delete-trailing-whitespace))


;; apply features
(dolist (mode-hook dev-mode-hook-list)
  (dolist
      (feature
       '(highlight-symbol-mode-on
         hs-minor-mode
         my-delete-trailing-space
         my-untabify))
    ;; makefile里面用TAB来标记命令，所以不能删除tab
    (if (not (or
              ;; no untabify in makefile
              (and (equal mode-hook 'makefile-mode-hook)
                   (equal feature 'my-untabify))
              ;; no untabify in c/c++ mode to avoid svn howls. sick!
              (and (dw-on-office-machine)
                   (equal mode-hook 'c-mode-common-hook)
                   (or (equal feature 'my-untabify)
                       (equal feature 'my-delete-trailing-space)))
              (and (equal mode-hook 'markdown-mode-hook)
                   (equal feature 'my-delete-trailing-space))))
        (add-hook mode-hook feature))))


(defun dev-misc()
  "Miscellaneous settings for software development."

  ;; set tab width
  (setq default-tab-width 4)
  (setq tab-width 4)

  ;; 用空格而不用tab来对齐(默认用tab来对齐)
  (setq-default indent-tabs-mode nil)

  (require 'paren-settings)
  )

(dev-misc)


(require 'occur-settings)

(require 'emacs-lisp-mode-settings)
(require 'scheme-mode-settings)
(require 'common-lisp-mode-settings)

(require 'wlc-settings)
(require 'rainbow-delimiters-settings)

(require 'cc-mode-settings)
(require 'c-mode-settings)
(require 'c++-mode-settings)

(require 'makefile-mode-settings)

(require 'java-mode-settings)
;;(require 'jde-settings)

(require 'sh-mode-settings)
(require 'awk-mode-settings)

(require 'shell-mode-settings)
(require 'multi-term-settings)

(require 'python-mode-settings)

(require 'erlang-mode-settings)

(require 'thrift-mode-settings)
(require 'markdown-mode-settings)

(require 'conf-mode-settings)

;; flymake
;;(require 'flymake-settings)

;; cedet
(require 'cedet-settings)
(require 'eassist-settings)

;; 把speedbar放到当前frame里面
(require 'sr-speedbar-settings)

;; 把imenu以tree的形式显示出来
(require 'imenu-tree-settings)

;; ecb 代码浏览器
(require 'ecb-settings)
(require 'tree-buffer-settings)

;; global
(require 'gtags-settings)

;; 所有的自动补全的配置
(require 'auto-complete-settings-all)


(provide 'dev-settings)
