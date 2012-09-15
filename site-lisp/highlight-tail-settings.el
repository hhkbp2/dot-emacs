;; -*- Emacs-Lisp -*-
;; Settings for `highlight-tail-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-01-30 00:20>

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
;; `highlight-tail-mode'
;; 高亮标记最近的修改

;;; Code:


(require 'highlight-tail)

(require 'dev-base)


(defun highlight-tail-settings ()
  "Settings for `highlight-tail-mode'."

  ;; 设置highlight-tail-colors控制渐变颜色,
  ;; a. 适合白底黑字
  ;;(setq highlight-tail-colors
  ;;        '(("#c1e156" . 0)
  ;;          ("#b8ff07" . 25)
  ;;          ("#00c377" . 60)))
  ;; b. 适合黑底白字
  ;; b1. 单色
  ;; (setq highlight-tail-colors
  ;;           '(("black" . 0)
  ;;             ("#bc2525" . 25)
  ;;             ("black" . 66)))
  ;; b2. 混色
  (setq highlight-tail-colors
        '(("black" . 0)
          ("red" . 40)
          ("blue" . 80)
          ("black" . 100)))
  ;; b3. 华丽，适合黑底白字
  ;; (setq highlight-tail-colors
  ;;       '(("black" . 0)
  ;;         ("green" . 20)
  ;;         ("blue" . 40)
  ;;         ("yellow" . 60)
  ;;         ("red" . 80)))
  )

(eval-after-load "highlight-tail"
  `(highlight-tail-settings))


;; 自动加载
(dolist (mode-hook dev-mode-hook-list)
  (add-hook mode-hook
            'highlight-tail-mode))


(provide 'highlight-tail-settings)
