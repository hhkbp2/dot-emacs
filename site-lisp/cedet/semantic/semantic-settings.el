;; -*- Emacs-Lisp -*-
;; Settings for `semantic'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-06-28 01:34>

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


;; load face settings
(require 'semantic-face-settings)


(defun semantic-setup-settings ()
  "Settings for semantic setup."

  ;; enabling semantic (code-parsing, smart completion) features
  ;; select one of the following:

  ;; * this enables the database and idle reparse engines
  ;; * set the tag store directory for semantic
  ;; (semantic-load-enable-minimum-features)

  ;; * this enables some tools useful for coding
  ;; * such as summary mode, imenu support, and the semantic navigator
  (semantic-load-enable-code-helpers)

  ;; * this enables even more coding tools, such as intellisense mode
  ;; * decoration mode, and stickyfunc mode (plus regular code helpers)
  ;;(semantic-load-enable-gaudy-code-helpers)

  ;; * TODO: add comment
  ;;(semantic-load-enable-excessive-code-helpers)

  ;; * TODO: add comment
  ;;(semantic-load-enable-semantic-debugging-helpers)

  (semantic-idle-scheduler-mode -1)


  ;; 在非终端(X窗口)下打开semantic的代码折叠功能
  (when window-system
    (global-semantic-tag-folding-mode 1))
  )


(defun my-semantic-ia-fast-jump-back ()
  "Jump back to the position before previous semantic tag jumping.
This is a completement of `semantic-ia-fast-jump'."
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
         (alist (semantic-mrub-ring-to-assoc-list ring))
         (first (cdr (car alist))))
    (if (semantic-equivalent-tag-p (oref first tag)
                                   (semantic-current-tag))
        (setq first (cdr (car (cdr alist)))))
    (semantic-mrub-switch-tags first)))


(defun semantic-settings ()
  "Settings for `semantic'."

  ;; set tag store directory for semantic
  (setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")

  ;; system include path
  ;; (setq semanticdb-project-roots (list (expand-file-name "/")))

  ;; enable support for gnu global
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  ;; key bindings
  ;; 跳转到定义
  (global-set-key [f7] 'semantic-ia-fast-jump)

  ;; 跳转到定义后跳转回来
  (global-set-key [f8] 'my-semantic-ia-fast-jump-back)

  ;; 在函数声明与定义之间跳转
  (define-key c-mode-base-map [(control f7)] 'semantic-analyze-proto-impl-toggle)

  ;; 代码补全
  (define-key c-mode-base-map (kbd "M-/")
    (if window-system
        (quote semantic-ia-complete-symbol-menu)
      (quote semantic-ia-complete-symbol)))

  (setq semantic-idle-work-update-headers-flag nil)
  (setq semantic-idle-work-parse-neighboring-files-flag nil)
  )


(eval-after-load "semantic"
  `(semantic-settings))


(provide 'semantic-settings)
