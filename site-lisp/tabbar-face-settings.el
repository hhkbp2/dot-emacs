;; -*- Emacs-Lisp -*-
;; Face settings for `tabbar-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2013-01-01 22:05>

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


(require 'tabbar)


(defun tabbar-face-settings ()
  "Face settings for `tabbar-mode'."
  (custom-set-faces
   ;; 设置默认主题: 背景和前景颜色，大小，字体
   '(tabbar-default
     ((((class color) (min-colors 88))
       (:background "gray80" :foreground "gray30" :height 0.9 :weight normal))
      (((class color) (min-colors 16))
       (:background "gray80" :foreground "gray30" :weight normal))
      (((class color) (min-colors 8))
       (:background "white" :foreground "black" :weight normal))
      (((type tty) (class mono))
       (:background "white" :foreground "black" :weight normal))
      (t (:background "gray80" :foreground "gray30"
                      :height 0.9 :weight normal))))
   ;; 设置左边按钮外观：外框框边大小和颜色
   '(tabbar-button
     ((((class color) (min-colors 88))
       (:inherit tabbar-default :foreground "dark red"
                 :box (:line-width 1 :color "gray30")))
      (((class color) (min-colors 16))
       (:inherit tabbar-default :foreground "dark red"
                 :box (:line-width 1 :color "gray30")))
      (((class color) (min-colors 8))
       (:inherit tabbar-default :foreground "red"))
      (((type tty) (class mono))
       (:inherit tabbar-default :foreground "red"))
      (t (:inherit tabbar-default :foreground "dark red"
                   :box (:line-width 1 :color "gray30")))))
   '(tabbar-button-highlight
     ((t (:inherit tabbar-default))))
   '(tabbar-highlight
     ((t (:underline t))))
   ;; 设置当前tab外观：颜色，字体，外框框边大小和颜色
   '(tabbar-selected
     ((((class color) (min-colors 88))
       (:inherit tabbar-default :background "LightGoldenrod"
                 :foreground "DarkGreen"
                 :box (:line-width 2 :color "DarkGoldenrod")
                 :weight bold))
      (((class color) (min-colors 16))
       (:inherit tabbar-default
                 :background "LightGoldenrod" :foreground "DarkGreen"
                 :weight bold))
      (((class color) (min-colors 8))
       (:inherit tabbar-default :background "black" :foreground "green"))
      (((type tty) (class mono))
       (:inherit tabbar-default :background "black" :foreground "green"))
      (t (:inherit tabbar-default :background "LightGoldenrod"
                   :foreground "DarkGreen"
                   :box (:line-width 2 :color "DarkGoldenrod")
                   :weight bold))))
   '(tabbar-separator
     ((((class color) (min-colors 88))
       (:inherit tabbar-default :height 0.1))
      (((class color) (min-colors 16))
       (:inherit tabbar-default :height 0.1))
      (((class color) (min-colors 8))
       (:inherit tabbar-default :foreground "black"))
      (((type tty) (class mono))
       (:inherit tabbar-default :foreground "black"))
      (t (:inherit tabbar-default :height 0.1))))
   ;; 设置非当前tab外观：外框框边大小和颜色
   '(tabbar-unselected
     ((((class color) (min-colors 88))
       (:inherit tabbar-default :box (:line-width 2 :color "gray70")))
      (((class color) (min-colors 16))
       (:inherit tabbar-default :box (:line-width 2 :color "gray70")))
      (((class color) (min-colors 8))
       (:inherit tabbar-default :foreground "black"))
      (((type tty) (class mono))
       (:inherit tabbar-default :foreground "black"))
      (t (:inherit tabbar-default :box (:line-width 2 :color "gray70"))))
     ))

  ;; 设置终端下的seperator的格式和大小
  (if (equal window-system
             nil) ;; nil means text-only-terminal
      ;; refer to `tabbar.el' for more information
      ;; to set separator widget properly
      ;; I prefer to separate tabs with special chars or
      ;; 1 character width space
      (setq tabbar-separator
            ;;or (list 1.0)
            (list "} {")))
  )


(eval-after-load "tabbar"
  '(tabbar-face-settings))


(provide 'tabbar-face-settings)
