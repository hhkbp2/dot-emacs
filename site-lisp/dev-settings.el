;; -*- Emacs-Lisp -*-
;; Settings for software development.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-10-22 23:28>

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


;;; 配置右括号自动补全
(defun my-auto-complete-pair ()
  "Automatically complete the right part of pairs used in all programming modes.
\(Except all lisp modes, for which there is a separat setting in `wlc'.
See `wlc/auto-complete-pair'.)
The pairs include '', \"\", [], (), {}."

  (make-local-variable 'skeleton-pair)
  (make-local-variable 'skeleton-pair-on-word)
  (make-local-variable 'skeleton-pair-filter-function)
  (make-local-variable 'skeleton-pair-alist)

  ;; enable auto pair
  (setq skeleton-pair t)
  ;; don't auto complete pair before or inside a word
  (setq skeleton-pair-on-word nil)
  ;; enable no checking before inserting the complemental pair
  (setq skeleton-pair-filter-function (lambda () nil))
  ;; customize the complete part on each left part of pairs inserting
  (setq skeleton-pair-alist
        '((?' _ "'")
          (?\" _ "\"")
          (?\[ _ "]")
          (?\( _ ")")
          (?{ > _ ?})))
  ;; customize which key triggers the auto complete action
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))

(defun my-auto-complete-indent-pair ()
  "Automatically complete the right part of pairs(with indentation)."

  ;; override the default {} pair auto-completion setting
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist
        (append '((?{ > \n > _ \n ?} >))
                (assq-delete-all '?{ skeleton-pair-alist))))

;; 为部分编程mode加载右括号自动缩进补全
(dolist (mode-hook dev-mode-hook-list-static)
  (add-hook mode-hook 'my-auto-complete-indent-pair))

;; 为除各种lisp mode外的编程mode加载右括号自动补全
(dolist (mode-hook dev-mode-hook-list-nonlisp)
  (add-hook mode-hook 'my-auto-complete-pair))

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
                       (equal feature 'my-delete-trailing-space)))))
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

(require 'wlc-settings)

(require 'emacs-lisp-mode-settings)
(require 'scheme-mode-settings)
(require 'common-lisp-mode-settings)

(require 'cc-mode-settings)
(require 'c-mode-settings)
(require 'c++-mode-settings)

(require 'makefile-mode-settings)

(require 'java-mode-settings)
(require 'jde-settings)

(require 'sh-mode-settings)
(require 'awk-mode-settings)

(require 'shell-mode-settings)
(require 'multi-term-settings)

(require 'python-mode-settings)

(require 'erlang-mode-settings)

(require 'thrift-mode-settings)
(require 'markdown-settings)

(require 'conf-mode-settings)

;; flymake
(require 'flymake-settings)

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
