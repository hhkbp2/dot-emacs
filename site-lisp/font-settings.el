;; -*- Emacs-Lisp -*-
;; Settings for font.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-12-12 08:28>

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


(require 'cl)


(defconst dw-favor-en-font-list
  '("Envy Code R"
    "Monaco"
    "Menlo"
    "Source Code Pro"
    "Ubuntu Mono"
    "WenQuanYi Micro Hei")
  "Personal favor font list for language en in descendent order.")

(defconst dw-favor-font-size 15
  "Personal favor font size for all kinds of font.")

(defconst dw-favor-zh-font-list
  '("Hiragino Sans GB"
    "微软雅黑Monaco"
    "WenQuanYi Micro Hei")
  "Personal favor font list for language zh in descendent order.")


(defun dw-font-exsits-p (font-name-pattern)
  "Return t if any available font matches `font-name-patterm', else nil."
  (x-list-fonts font-name-pattern))

(defun dw-first-available-font (font-list)
  "Return first available font in `font-list'."
  (find-if #'dw-font-exsits-p font-list))


(defconst dw-default-font-name
  (if (display-graphic-p)
      (dw-first-available-font dw-favor-en-font-list)
    (first dw-favor-en-font-list))
  "Default frame font name.")

(defconst dw-default-font
  (format "%s-%s" dw-default-font-name dw-favor-font-size)
  "Default frame font desc.")


(defun dw-set-font (en-font-list
                    en-font-size
                    zh-font-list
                    &optional zh-font-size)
  "Set font for language en and zh.
`en-font-list' and `zh-font-list' should be a list of font name string.
`en-font-size' should be of integer. If `zh-font-size' if omitetd,
its value comes from `en-font-size'."

  (or zh-font-size (setq zh-font-size en-font-size))
  (let* ((en-font-name (dw-first-available-font en-font-list))
         (zh-font-name (dw-first-available-font zh-font-list)))
    (unless (and en-font-name zh-font-name)
      (error "no available font in font list en: %s and zh: %s"
             en-font-list zh-font-list))
    ;; set en font
    (set-face-attribute 'default nil
                        :font (format "%s %s" en-font-name en-font-size))
    ;; set zh font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family zh-font-name :size zh-font-size)))))

(defun dw-set-frame-font ()
  "Set the frame default font."
  (set-frame-font dw-default-font)
  (add-to-list 'default-frame-alist `(font . ,dw-default-font)))


(defun font-settings ()
  "Settings for font."

  (when (display-graphic-p)
    ;; font setting
    (dw-set-frame-font)
    (dw-set-font dw-favor-en-font-list
                 dw-favor-font-size
                 dw-favor-zh-font-list
                 (1- dw-favor-font-size))))


(provide 'font-settings)
