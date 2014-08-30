;; -*- Emacs-Lisp -*-
;; Settings related to frame.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-08-30 19:40>

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


(defun dw-frame-settings ()
  "Settings for new frame."

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

  ;; 应用font配置
  (font-settings)

  ;; set initial frame size
  ;;(setq initial-frame-alist '((width . 80) (height . 30)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))
  ;; set default frame size
  ;;(setq default-frame-alist '((width . 75) (height . 25)
  ;;                            (menu-bar-lines . 0) (tool-bar-lines . 0)))
  ;; 启动Emacs的时候最大化Emacs
  ;;(require 'maxframe-settings)

  )


(defun frame-settings ()
  "Settings related to frame."

  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                '(lambda (new-frame)
                   (select-frame new-frame)
                   (dw-frame-settings)))
    (dw-frame-settings))
  )


(provide 'frame-settings)
