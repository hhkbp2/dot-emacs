;; -*- Emacs-Lisp -*-
;; Settings for `conf-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-03-03 01:37>

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

;;; Code:


(defun conf-mode-settings ()
  "Settings for `conf-mode'."
  ;; 定制`conf-mode'环境
  ;; 以两个!!开始
  (setq comment-start "!!")
  )

(add-hook 'conf-mode-hook
          'conf-mode-settings)


(provide 'conf-mode-settings)