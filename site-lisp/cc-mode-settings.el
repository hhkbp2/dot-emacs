;; -*- Emacs-Lisp -*-
;; Settings for `cc-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-09-26 10:39>

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
(require 'dw-functionals)


(defun cc-mode-settings ()
  "Settings for `cc-mode'."

  ;; 定制`cc-mode'环境
  ;; 缩进的宽度在自定义的缩进风格中设置
  (setq c-basic-offset 'set-from-style)

  ;; ;; 自动开始新行`auto-newline', default keybinding(on/off) "C-c C-a"
  ;; (c-toggle-auto-state)
  ;; ;; 饥饿的删除键`hungry-delete-key', default keybinding(on/off) "C-c C-d"
  (c-toggle-hungry-state)
  ;; 以上两种功能之和, default keybinding(on/off) "C-c C-t"
  ;;(c-toggle-auto-hungry-state)

  (if (dw-on-office-machine)
      (c-subword-mode 1)
    (subword-mode 1))

  ;; 控制执行`indent-new-comment-line'(keybinding M-j)是否在下一行新建一条注释
  (setq comment-multi-line nil)

  ;; 在状态条上显示当前光标在哪个函数体内部
  ;;(which-function-mode)

  ;; 预处理设置
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp -C")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq abbrev-mode t)

  ;; 定制注释风格
  (setq comment-start "// "
        comment-end "")

  ;; keybindings
  ;;(define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  ;; 将回车代替C-j的功能，换行的同时对齐
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  ;;(define-key c-mode-base-map [(f7)] 'compile)
  ;;(define-key c-mode-base-map [(meta \`)] 'c-indent-command)

  ;; define key sequences for comment and uncomment
  (define-key c-mode-base-map [(control c) (k)] 'kill-sexp)
  (define-key c-mode-base-map [(control c) (control k)] 'kill-sexp)
  (define-key c-mode-base-map [(control c) (j)] 'backward-kill-sexp)
  (define-key c-mode-base-map [(control c) (control j)] 'backward-kill-sexp)
  (define-key c-mode-base-map [(control c) (m)] 'mark-sexp)
  (define-key c-mode-base-map [(control c) (control m)] 'mark-sexp)
  (define-key c-mode-base-map [(control c) (c)] 'comment-dwim)
  (define-key c-mode-base-map [(control c) (control c)] 'comment-dwim)
  )

(add-hook 'c-mode-common-hook
          'cc-mode-settings)


(provide 'cc-mode-settings)
