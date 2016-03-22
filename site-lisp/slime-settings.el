;;; slime-settings.el --- Settings for `slime'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:57>

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


(require 'slime-autoloads)
(require 'wlc)


(defun slime-settings ()
  "Settings for `slime'."

  ;; 使用sbcl作为common lisp实现
  (setq inferior-lisp-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/sbcl"
          "/usr/bin/sbcl"))
  (slime-setup `(slime-repl slime-fancy slime-fuzzy))
  (setq slime-net-coding-system 'utf-8-unix)
  )

(defun slime-repl-settings ()
  "Settings for `slime-repl-mode'."

  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
  )

(eval-after-load "slime"
  `(slime-settings))

(eval-after-load "slime-repl"
  `(slime-repl-settings))


(provide 'slime-settings)

;;; slime-settings.el ends here
