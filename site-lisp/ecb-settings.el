;; -*- Emacs-Lisp -*-
;; Settings for `ecb'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 14:10>

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


(require 'ecb-autoloads)
;; load face settings
(require 'ecb-face-settings)


(defun ecb ()
  "Start ecb as customized."
  (interactive)

  ;; 启动前配置
  (setq
   ;; 启动时不弹出tip提示窗口
   ecb-tip-of-the-day nil
   ;; 启动时不做兼容检查
   ecb-auto-compatibility-check nil
   ;; 启动不做版本检查
   ecb-version-check nil)

  ;; 启动ecb
  (ecb-activate)

  ;; 启动后配置
  ;; 自定义ecb窗口宽度，只对left-,right-风格有效
  (custom-set-variables
   ;; 部分ecb属性不能用setq设置，参考ecb文档
   ;; http://www.xemacs.org/Documentation/packages/html/ecb_5.html#SEC77
   '(ecb-windows-width 0.2)
   '(ecb-show-sources-in-directories-buffer
     (cons "eclipse-like" ecb-show-sources-in-directories-buffer)))

  ;; 自定义ecb窗口布局
  ;; 有两种方法：
  ;; 1. 推荐使用命令ecb-create-new-layout，在交互过程中指定窗口布局和类型，参考
  ;;    http://ecb.sourceforge.net/docs/Creating-a-new-ECB_002dlayout.html#fn-1
  ;; 2. 使用函数ecb-layout-define，参考
  ;;    ecb文档http://ecb.sourceforge.net/docs/Programming-a-new-layout.html
  ;;    和ecb定义自带left-, right-, top-风格的窗口布局定义源代码
  ;;    后者见ecb包文件ecb-layout-defs.el
  (ecb-layout-define "eclipse-like" left-right
    "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |   Methods   |
   |  and sources |                               |             |
   |  tree        |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |             Edit              |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |-------------|
   |              |                               |             |
   |              |                               |   History   |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows
get a little more place."
    (ecb-set-directories-buffer)
    (select-window (next-window (next-window)))
    (ecb-set-methods-buffer)
    (ecb-split-ver 0.80)
    (ecb-set-history-buffer)
    (select-window
     (previous-window
      (previous-window (selected-window) 0) 0)))

  (ecb-layout-define "eclipse-speedbar" left-right
    "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Speedbar    |                               |   Methods   |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |             Edit              |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows
get a little more place."
    (ecb-set-speedbar-buffer)
    (select-window (next-window (next-window)))
    (ecb-set-methods-buffer)
    (select-window
     (previous-window (selected-window) 0)))


  (ecb-layout-define "mono-speedbar" left
    "This function creates the following layout:

   --------------------------------------------------------------
   |              |                                             |
   |  Speedbar    |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                     Edit                    |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   |              |                                             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows
get a little more place."
    (ecb-set-speedbar-buffer)
    (select-window (next-window)))


  (ecb-layout-define "mono-methods" right
    "This function creates the following layout:

   --------------------------------------------------------------
   |                                              |             |
   |                                              |   Methods   |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                    Edit                      |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   |                                              |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows
get a little more place."
    (let ((edit-win (previous-window (selected-window) 0)))
      (ecb-set-methods-buffer)
      (select-window edit-win)))


  (ecb-layout-switch "mono-speedbar"))


(defconst ecb-layout-window-sizes-eclipse-like-console
  '(("eclipse-like"
     (0.13793103448275862 . 0.9787234042553191)
     (0.12931034482758622 . 0.7446808510638298)
     (0.12931034482758622 . 0.23404255319148937)))
  "Settings of `ecb-layout-window-sizes' for \"eclipse-like\" layout
under console.")


(defconst ecb-layout-window-sizes-eclipse-like-x
  '(("eclipse-like"
     (0.22857142857142856 . 0.9803921568627451)
     (0.16428571428571428 . 0.7450980392156863)
     (0.16428571428571428 . 0.23529411764705882)))
  "Settings of `ecb-layout-window-sizes' for \"eclipse-like\" layout
under window system.")


(defconst ecb-layout-window-sizes-eclipse-speedbar-console
  '(("eclipse-speedbar"
     (0.14655172413793102 . 0.9787234042553191)
     (0.1206896551724138 . 0.9787234042553191)))
  "Settings of `ecb-layout-window-sizes' for \"eclipse-speedbar\" layout
under console.")


(defconst ecb-layout-window-sizes-eclipse-speedbar-x
  '(("eclipse-speedbar"
     (0.19285714285714287 . 0.9803921568627451)
     (0.20714285714285716 . 0.9803921568627451)))
  "Settings of `ecb-layout-window-sizes' for \"eclipse-speedbar\" layout
under window system.")


(defconst ecb-layout-window-sizes-mono-speedbar
  '(("mono-speedbar"
     (0.2672413793103448 . 0.9787234042553191)))
  "Settings of `ecb-layout-window-sizes' for \"mono-speedbar\" layout.")


(defconst ecb-layout-window-sizes-mono-methods
  '(("mono-methods"
     (0.30714285714285716 . 0.9803921568627451)))
  "Settings of `ecb-layout-window-sizes' for \"mono-methods\" layout.")


(defadvice ecb-layout-switch (after my-ecb-adjust-window-sizes)
  "Adjust window sizes according to layout style."
  (let (window-sizes)
    (cond
     ((equal ecb-layout-name "eclipse-like")
      (setq window-sizes
            (if window-system
                ecb-layout-window-sizes-eclipse-like-x
              ecb-layout-window-sizes-eclipse-like-console)))
     ((equal ecb-layout-name "eclipse-speedbar")
      (setq window-sizes
            (if window-system
                ecb-layout-window-sizes-eclipse-speedbar-x
              ecb-layout-window-sizes-eclipse-speedbar-console)))
     ((equal ecb-layout-name "mono-speedbar")
      (setq window-sizes ecb-layout-window-sizes-mono-speedbar))
     ((equal ecb-layout-name "mono-methods")
      (setq window-sizes ecb-layout-window-sizes-mono-methods))
     (t))
    (when window-sizes
      (custom-set-variables
       '(ecb-layout-window-sizes window-sizes 'EVAL-NOW)))))


(defun ecb-settings ()
  "Settings for `ecb'."
  (custom-set-variables
   '(ecb-options-version "2.40")
   ;; 将默认mouse 2鼠标中键打开树结点改为mouse 1鼠标左键
   '(ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
   '(ecb-source-path
     '(("~/elisp" "lisp")
       ("~/rep/program/" "prog")))
   '(ecb-history-make-buckets 'never)
   '(ecb-history-sort-method nil))

  (ad-activate 'ecb-layout-switch)

  (define-key ecb-mode-map [(control c) (w) (d)] 'ecb-goto-window-directories)
  (define-key ecb-mode-map [(control c) (w) (s)] 'ecb-goto-window-speedbar)
  (define-key ecb-mode-map [(control c) (w) (e)] 'ecb-goto-window-edit-last)
  (define-key ecb-mode-map [(control c) (w) (m)] 'ecb-goto-window-methods)
  (define-key ecb-mode-map [(control c) (w) (h)] 'ecb-goto-window-history)
  (define-key ecb-mode-map [(control c) (l) (c)] 'ecb-change-layout)
  (define-key ecb-mode-map [(control c) (l) (e)]
    (lambda ()
      (interactive)
      (ecb-layout-switch "eclipse-like")))
  (define-key ecb-mode-map [(control c) (l) (s)]
    (lambda ()
      (interactive)
      (ecb-layout-switch "eclipse-speedbar")))
  (define-key ecb-mode-map [(control c) (l) (r)]
    (lambda ()
      (interactive)
      (ecb-layout-switch "mono-speedbar")))
  (define-key ecb-mode-map [(control c) (l) (m)]
    (lambda ()
      (interactive)
      (ecb-layout-switch "mono-methods")))

  (global-set-key [(control c) (s) (d)] 'ecb-deactivate)
  )

(global-set-key [(control c) (s) (e)] 'ecb)


(eval-after-load "ecb"
  `(ecb-settings))


(provide 'ecb-settings)