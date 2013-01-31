;; -*- Emacs-Lisp -*-
;; Settings for appearance.

;; Copyright (C) 2009, 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2013-01-29 00:18>

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


(defun appearance-settings ()
  "Settings for appearance."

  ;; close startup message
  (setq inhibit-startup-message t)

  ;; disable visible bell (and the noisy warning bell)
  (setq visible-bell nil)

  ;; hide menu-bar under terminal
  (if (equal window-system nil)
      (menu-bar-mode nil))

  ;; hide tool-bar
  (tool-bar-mode nil)

  ;; hide scroll-bar
  (scroll-bar-mode nil)

    ;; make scroll movement more comfortable
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 10000)

  ;; 用滚轴鼠标
  (mouse-wheel-mode t)

  ;; don't blink the cursor
  (blink-cursor-mode nil)

  ;; mouse will jump away if the cursor is near it
  (mouse-avoidance-mode 'animate)

  ;; highlight marked region
  (setq-default transient-mark-mode t)


  ;; split the window horizontally when use ediff
  (setq ediff-split-window-function 'split-window-horizontally)


  ;; set initial frame size
  ;;(setq initial-frame-alist '((width . 80) (height . 30)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))
  ;; set default frame size
  ;;(setq default-frame-alist '((width . 75) (height . 25)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))


  ;; font setting
  (set-fontset-font "fontset-default"
                    'unicode '("Ubuntu Mono-12" . "unicode-bmp"))
  (add-to-list 'default-frame-alist
               '(font . "Ubuntu Mono-12"))
  (set-frame-font "Ubuntu Mono-12")

  ;; 加载font-lock配置
  (require 'font-lock-settings)

  ;; 设置背景景色为暗色以配合主题
  ;;(custom-set-variables '(frame-background-mode (quote dark)))

  (require 'basic-faces-settings)

  ;; load `color-theme' settings
  (require 'color-theme-settings)
  ;; load my favorite color theme
  (require 'color-theme-darkmate)
  (color-theme-darkmate)

  ;; 启动Emacs的时候最大化Emacs
  ;;(require 'maxframe-settings)
  )

(appearance-settings)


(provide 'appearance-settings)
