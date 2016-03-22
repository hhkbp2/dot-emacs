;;; java-mode-settings.el --- Settings for `java-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:58>

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


(require 'cc-mode)


(defun java-mode-settings ()
  "Settings for `java-mode'."

  ;; set indentation style to "java"
  (c-set-style "java")
  ;; enable c-subword-mode
  (if (dw-version->=-23.3)
      (subword-mode 1)
    (c-subword-mode 1))

  (local-set-key [(control c) (c)] 'comment-dwim)
  (local-set-key [(control c) (control c)] 'comment-dwim))


(add-hook 'java-mode-hook
          'java-mode-settings)


(provide 'java-mode-settings)

;;; java-mode-settings ends here
