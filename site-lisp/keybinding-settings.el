;;; keybinding-settings.el --- Settings for key binding
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2009, 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 12:00>

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
;; Key binding 设置键帮定
;; 尽量放在.emacs文件的最后，以免被绑定的键不小心被后面的配置文件覆盖

;;; Code:


(defun dw-mac-set-func-key-default ()
  "Set command and option key on mac to default."
  ;; set the functional key mapping on mac to default
  ;;     command -> command
  ;;     option -> meta
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

(defun dw-mac-set-func-key-swap ()
  "Switch command and option key on mac."
  ;; switch key settings on mac to personal favour
  ;;     command -> meta
  ;;     option -> command
  (setq mac-option-modifier 'super)   ; `super' means command key
  (setq mac-command-modifier 'meta))

(defun keybinding-settings-for-mac ()
  "Settings for keybindings for mac os."

  (dw-mac-set-func-key-default)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char))


(defun keybinding-settings ()
  "Settings for keybinding."

  ;; 很少需要挂起emacs，取消`suspend-frame'的默认键绑定
  ;; 键序列`C-z'容易与键序列`C-x'混淆
  (global-unset-key "\C-z")

  ;; 不需要用`F1'打开帮助，常用帮助有info和woman（就是在emacs中看man）
  ;;(global-set-key [f1] 'info)
  (global-unset-key [(f1)])
  ;; 取消默认的`kmacro-end-or-call-macro'键绑定
  (global-unset-key [(f4)])

  ;; undo and redo
  (global-set-key [f2] 'undo)
  (global-set-key [(f3)] 'redo)
  (global-set-key [(control q)] 'undo)
  (global-set-key [(control z)] 'redo)

  ;; 关闭当前buffer
  (global-set-key [f10] 'kill-this-buffer)

  ;; switch to previous buffer
  ;;(global-set-key [f11] 'previous-buffer)
  (global-set-key [(control x)(meta p)] 'previous-buffer)
  ;; switch to next buffer
  ;;(global-set-key [f12] 'next-buffer)
  (global-set-key [(control x)(meta n)] 'next-buffer)

  ;; Home键 光标跳到文件开头
  (global-set-key [(home)] 'beginning-of-buffer)
  ;; End键 光标跳到文件结尾
  (global-set-key [(end)] 'end-of-buffer)

  ;; 用`ibuffer'代替默认的`buffer-menu'
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; 很少手动改`fill-column'的值，取消其默认键绑定
  ;; 键序列`C-x f'容易和键序列`C-x C-f'混淆
  (global-unset-key [(control x) (f)])
  (global-set-key [(control x) (f)] 'find-file)

  ;; 更改退出emacs的默认键绑定
  ;; 键序列`C-x C-c'容易与键序列`C-x C-x'混淆
  ;; `C-x' `C-c'也是很多快捷键的前缀，容易误按
  (global-unset-key [(control x) (control c)])
  (global-set-key [(control x) (meta q)] 'save-buffers-kill-terminal)

  (when (eq system-type 'darwin)
    (keybinding-settings-for-mac))
  )

(keybinding-settings)


(provide 'keybinding-settings)

;;; keybinding-settings.el ends here
