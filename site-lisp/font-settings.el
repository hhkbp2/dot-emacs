;; -*- Emacs-Lisp -*-
;; Settings for font.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-08-31 23:55>

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


(defconst dw-favor-en-font-list
  '("M+ 1mn"
    "Envy Code R"
    "Monaco"
    "Menlo"
    "Source Code Pro"
    "Ubuntu Mono"
    "Fantasque Sans Mono"
    )
  "Personal favor font list for language en in descendent order.")

(defconst dw-favor-zh-font-list
  '("Hiragino Sans GB"
    "WenQuanYi Micro Hei Mono"
    "微软雅黑Monaco"
    )
  "Personal favor font list for language zh in descendent order.")

(setq dw-font-pairs
      '((("M+ 1mn" . "Hiragino Sans GB") .
         ;; font-spec parameters, rescale ratio
         (((:size 15) . 1.0) . ((:size 15) . 1.1)))
        (("Monaco" . "Hiragino Sans GB") .
         (((:size 14) . 1.0) . ((:size 14) . 1.2)))
        (("Envy Code R" . "Hiragino Sans GB") .
         (((:size 15) . 1.0) . ((:size 15) . 1.1)))
        (("Source Code Pro" . "Hiragino Sans GB") .
         (((:size 15) . 1.0) . ((:size 15) . 1.2)))
        (("Ubuntu Mono" . "Hiragino Sans GB") .
         (((:size 16) . 1.0) . ((:size 16) . 1.0)))
        (("Fantasque Sans Mono" . "Hiragino Sans GB") .
         (((:size 16) . 1.0) . ((:size 16) . 1.0)))
        (("M+ 1mn" . "WenQuanYi Micro Hei Mono") .
         (((:size 15) . 1.0) . ((:size 15) . 1.1)))
        ))


(defun dw-font-exsits-p (font-name-pattern)
  "Return t if any available font matches `font-name-pattern', else nil."
  (x-list-fonts font-name-pattern))

(defun dw-first-available-font (font-list)
  "Return first available font in `font-list'."
  (find-if #'dw-font-exsits-p font-list))

(defun dw-font-pair-exists-p (pair)
  (assoc pair dw-font-pairs))

(defun dw-available-font-pair (en-font-list zh-font-list)
  (let* ((en-font-name (dw-first-available-font en-font-list))
         (zh-font-name (dw-first-available-font zh-font-list))
         (font-pair (cons en-font-name zh-font-name)))
    (unless (and en-font-name zh-font-name)
      (error "no available font in font list en: %s and zh: %s"
             en-font-list zh-font-list))
    (unless (dw-font-pair-exists-p font-pair)
      (error "no available font pair for font en: %s and zh: %s"
             en-font-name zh-font-name))
    font-pair))

(defun dw-font-spec-for-pair (pair)
  (let ((pair-parameters (cdr (assoc pair dw-font-pairs))))
    (cl-flet ((get-font-spec (name paras)
                             (apply #'font-spec :family name paras)))
      (cl-values (get-font-spec (car pair) (car (car pair-parameters)))
                 (get-font-spec (cdr pair) (car (cdr pair-parameters)))))))

(defun dw-font-rescale-alist-for-pair (pair)
  (let ((pair-parameters (cdr (assoc pair dw-font-pairs))))
    (cl-flet ((get-regexp (font-name)
                          (replace-regexp-in-string "\\s-" "." font-name)))
      (list  (cons (get-regexp (car pair)) (cdr (car pair-parameters)))
             (cons (get-regexp (cdr pair)) (cdr (cdr pair-parameters)))))))


(defun dw-set-font (en-font-list zh-font-list)
  "Set font for language en and zh.
`en-font-list' and `zh-font-list' should be a list of font name string.
It will try to find out the existing font in each list to perform a proper
settings according to `dw-font-pairs'."

  (let* ((font-pair (dw-available-font-pair en-font-list zh-font-list)))
    ;; simulate to multiple return values
    (cl-multiple-value-bind
        (en-font-spec zh-font-spec) (dw-font-spec-for-pair font-pair)
      ;; (set-face-attribute 'default t
      ;;                     :font en-font-spec  :weight 'normal :width 'normal
      ;; set en font
      (set-frame-font en-font-spec)
      ;; set zh font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          zh-font-spec)))))


(defadvice frame-notice-user-settings (before dw-rescale-alist)
  (let ((font-pair (dw-available-font-pair
                    dw-favor-en-font-list dw-favor-zh-font-list)))
    (dolist (elem (dw-font-rescale-alist-for-pair font-pair))
      (add-to-list 'face-font-rescale-alist
                   elem))))
(ad-activate 'frame-notice-user-settings)


(defun font-settings ()
  "Settings for font."

  (when (display-graphic-p)
    (setq-default line-spacing nil)
    (dw-set-font dw-favor-en-font-list dw-favor-zh-font-list)))


(provide 'font-settings)
