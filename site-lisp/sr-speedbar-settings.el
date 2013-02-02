;; -*- Emacs-Lisp -*-
;; Settings for `sr-speedbar'.

;; Copyright (C) 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-04 17:26>

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


(require 'sr-speedbar)


(defun sr-speedbar-settings ()
  "Settings for `sr-speedbar'."

  (setq
   ;; set its window's initial width under WINDOW system
   sr-speedbar-width-x 30
   ;; set its window's initial width under CONSOLE
   sr-speedbar-width-console 30
   ;; set its window's max width as 40% of frame
   sr-speedbar-max-width 40
   ;; skip `sr-speedbar' window when call `other-window'.
   ;;sr-speedbar-skip-other-window-p t
   ;; auto refresh its content (default value)
   sr-speedbar-auto-refresh t
   ;; show `sr-speedbar' on the left side
   sr-speedbar-right-side nil
   )

  ;; key bindings
  (global-set-key [(control c) (s) (r)] 'sr-speedbar-toggle)
  )

(eval-after-load "sr-speedbar"
  `(sr-speedbar-settings))


(provide 'sr-speedbar-settings)
