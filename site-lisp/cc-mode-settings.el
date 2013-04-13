;; -*- Emacs-Lisp -*-
;; Settings for `cc-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-04-13 16:24>

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


(defconst dw-cc-mode-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)
                               (namespace-open after)
                               (class-open after)
                               (extern-lang-open after)
                               (inexpr-class-open after)))
    (c-hanging-colons-alist . ((label after)
                               (access-label after)
                               (member-init-intro before)))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       scope-operator
                       compact-empty-funcall))
    (c-offsets-alist . (;; top indentation
                        (topmost-intro . 0)
                        (topmost-intro-cont . c-lineup-topmost-intro-cont)
                        ;; -- macro --
                        (cpp-macro . 0)
                        ;;(cpp-macro-cont . )
                        ;; -- function --
                        (defun-open . 0)
                        (defun-close . 0)
                        (defun-block-intro . +)
                        ;; between function arglist and opening brace
                        (func-decl-cont . c-lineup-java-throws)
                        ;; k&r style
                        (knr-argdecl-intro . +)
                        (knr-argdecl . 0)
                        ;; argument
                        (arglist-intro . c-lineup-arglist-intro-after-paren)
                        (arglist-cont . c-lineup-gcc-asm-reg)
                        (arglist-cont-nonempty . c-lineup-arglist-intro-after-paren)
                        (arglist-close . c-lineup-arglist)
                        ;; -- block --
                        (block-open . 0)
                        (block-close . 0)
                        ;; -- enum or static array list --
                        (brace-list-open . 0)
                        (brace-list-close . 0)
                        (brace-list-intro . +)
                        (brace-list-entry . 0)
                        (brace-entry-open . 0)
                        ;; -- statement --
                        (statement . 0)
                        (statement-cont . +)
                        (statement-block-intro . +)
                        (statement-case-open . 0)
                        (statement-case-intro . +)
                        (substatement . +)
                        (substatement-open . 0)
                        (substatement-label . 2)
                        ;; label
                        (label . 2)
                        (case-label . +)
                        ;; control statement
                        (do-while-closure . 0)
                        (else-clause . 0)
                        ;; namespace
                        (namespace-open . 0)
                        (namespace-close . 0)
                        (innamespace . 0)
                        ;; -- class --
                        ;; inheritance list
                        (inher-intro . ++)
                        (inher-cont . c-lineup-multi-inher)
                        ;; opening brace
                        (class-open . 0)
                        (class-close . 0)
                        (inclass . ++)
                        ;; in-class inline
                        (inline-open . 0)
                        (inline-close . 0)
                        ;; label
                        (access-label . -)
                        ;; member initialization
                        (member-init-intro . ++)
                        (member-init-cont . c-lineup-multi-inher)
                        ;; friend
                        (friend . 0)
                        ;; -- extern --
                        (extern-lang-open . 0)
                        (extern-lang-close . 0)
                        (inextern-lang . +)
                        ;; -- template --
                        (template-args-cont . (c-lineup-template-args +))
                        ;; -- others --
                        ;; string
                        (string . c-lineup-dont-change)
                        ;; exception
                        (catch-clause . 0)
                        ;; comment
                        (comment-intro . c-lineup-comment)
                        ;; stream
                        (stream-op . c-lineup-streamop)
                        ;; expression inside
                        (inexpr-statement . +)
                        (inexpr-class . 0))))
  "Dylan.Wen's cc mode indentation style.")


(defmacro dw-apply-code-style (style)
  (let ((sym-name (symbol-name style)))
    `(progn
       (c-add-style ,sym-name ,style)
       (c-set-style ,sym-name))))


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

  (if (dw-version->=-23.3)
      (subword-mode 1)
    (c-subword-mode 1))

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

  (setq c-tab-always-indent t)
  (setq c-echo-syntactic-information-p t)
  ;; 定制缩进风格
  (dw-apply-code-style dw-cc-mode-style)

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
