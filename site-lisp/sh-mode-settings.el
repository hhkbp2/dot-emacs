;; -*- Emacs-Lisp -*-
;; Settings for `sh-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-01-21 17:21>

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
;; Settings for `sh-mode',
;; which is mainly for editing shell script.
;;

;;; Code:



(defun sh-mode-settings ()
  "Settings for `sh-mode'."

  ;; TODO add settings

  ;; set indentation style
  (setq sh-styles-alist
        '(("lw-sh-style"
           (sh-basic-offset . 4)
           (sh-first-lines-indent . 0)
           (sh-indent-after-case . +)
           (sh-indent-after-do . +)
           (sh-indent-after-done . 0)
           (sh-indent-after-else . +)
           (sh-indent-after-function . +)
           (sh-indent-after-if . +)
           (sh-indent-after-loop-construct . +)
           (sh-indent-after-open . +)
           (sh-indent-after-switch . +)
           (sh-indent-comment . t)
           (sh-indent-for-case-alt . ++)
           (sh-indent-for-case-label . +)
           (sh-indent-for-continuation . +)
           (sh-indent-for-do . 0)
           (sh-indent-for-done . 0)
           (sh-indent-for-else . 0)
           (sh-indent-for-fi . 0)
           (sh-indent-for-then . 0))))
  (sh-load-style "lw-sh-style")
  )

;; (eval-after-load "sh-script"
;;   `(sh-mode-settings))

(dolist (mode-hook
         '(sh-mode-hook))
  (add-hook mode-hook 'sh-mode-settings))


(provide 'sh-mode-settings)