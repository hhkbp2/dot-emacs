;; -*- Emacs-Lisp -*-
;; Settings related to frame.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-02-17 14:59>

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

  (when (display-graphic-p)
    ;; hide scroll-bar
    (scroll-bar-mode -1)
    ;; 应用font配置
    (font-settings))
  )


(defun frame-settings ()
  "Settings related to frame."

  ;; run this settings function for this frame.
  (dw-frame-settings)
  ;; hook this settings function to future frame creation.
  (add-hook 'after-make-frame-functions
                '(lambda (new-frame)
                   (select-frame new-frame)
                   (dw-frame-settings)))
  )


(provide 'frame-settings)
