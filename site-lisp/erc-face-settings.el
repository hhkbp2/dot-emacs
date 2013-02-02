;; -*- Emacs-Lisp -*-
;; Face settings for `erc'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 14:05>

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


(defun erc-face-settings ()
  "Face settings for `erc'."
  (custom-set-faces
   '(erc-action-face ((t (:weight semi-bold))))
   '(erc-bold-face ((t (:weight bold))))
   '(erc-current-nick-face
     ((t (:foreground "LightSeaGreen" :weight semi-bold))))
   '(erc-dangerous-host-face ((t (:foreground "#ff2f6a"))))
   '(erc-default-face ((t (nil))))
   '(erc-direct-msg-face ((t (:foreground "#ff44cc"))))
   '(erc-error-face
     ((t (:background "darkblue" :foreground "#ff2f6a" :weight semi-bold))))
   '(erc-fool-face ((t (:foreground "dim gray"))))
   '(erc-input-face ((t (:foreground "#009cff"))))
   '(erc-inverse-face
     ((t (:background "Darkgreen" :foreground "Black" :weight semi-bold))))
   '(erc-keyword-face ((t (:foreground "#ff9900" :weight bold))))
   '(erc-nick-default-face ((t (:weight semi-bold))))
   '(erc-nick-msg-face ((t (:foreground "#ff6100" :weight semi-bold))))
   '(erc-notice-face ((t (:foreground "#bb66ff" :weight normal))))
   '(erc-pal-face ((t (:foreground "MediumAquaMarine" :weight bold))))
   '(erc-prompt-face
     ((t (:background "black" :foreground "#ff9900" :weight semi-bold))))
   '(erc-timestamp-face ((t (:foreground "#96ff00" :weight normal))))
   '(erc-underline-face ((t (:underline t)))))
  )


(eval-after-load "erc"
  '(erc-face-settings))


(provide 'erc-face-settings)