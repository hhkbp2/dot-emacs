;; -*- Emacs-Lisp -*-
;; Settings for appearance.

;; Copyright (C) 2009, 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-05-15 12:21>

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


(require 'font-settings)
(require 'font-lock-settings)
(require 'basic-faces-settings)


(defun appearance-settings ()
  "Settings for appearance."

  ;; close startup message
  (setq inhibit-startup-message t)

  ;; disable visible bell (and the noisy warning bell)
  (setq visible-bell nil)

  ;; hide menu-bar under terminal
  (if (not (display-graphic-p))
      (menu-bar-mode -1))

  ;; hide tool-bar
  (tool-bar-mode -1)

  (if (display-graphic-p)
      ;; hide scroll-bar
      (scroll-bar-mode nil))

  ;; make scroll movement more comfortable
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 10000)

  (if (display-graphic-p)
      ;; 用滚轴鼠标
      (mouse-wheel-mode t))

  ;; don't blink the cursor
  (blink-cursor-mode nil)

  ;; mouse will jump away if the cursor is near it
  (mouse-avoidance-mode 'animate)

  ;; highlight marked region
  (setq-default transient-mark-mode t)

  ;; split the window horizontally when use ediff
  (setq ediff-split-window-function 'split-window-horizontally)

  (when (>= emacs-major-version 24)
    ;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
    (setq x-select-enable-clipboard t)
    ;; after mouse selection in X11, you can paste by `yank' in emacs
    (setq x-select-enable-primary t)
    )

  ;; set initial frame size
  ;;(setq initial-frame-alist '((width . 80) (height . 30)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))
  ;; set default frame size
  ;;(setq default-frame-alist '((width . 75) (height . 25)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))

  (encoding-settings)

  ;; 应用font配置
  (font-settings)

  ;; 加载font-lock配置
  (font-lock-settings)

  ;; 设置背景景色为暗色以配合主题
  ;;(custom-set-variables '(frame-background-mode (quote dark)))

  ;; load `color-theme' settings
  (require 'color-theme-settings)
  ;; load my favorite color theme
  (require 'color-theme-darkmate)
  (color-theme-darkmate)

  ;; 启动Emacs的时候最大化Emacs
  ;;(require 'maxframe-settings)

  (when (display-graphic-p)
    (require 'powerline-settings))
  )


(defun encoding-settings ()
  "Settings for encoding."
  (let ((prefer-coding-list '(utf-8 gbk gb2312 big5)))
    (dolist (coding (reverse prefer-coding-list))
      (prefer-coding-system coding)))
  (setq locale-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (set-language-environment-coding-systems "utf-8"))

(appearance-settings)


(provide 'appearance-settings)
