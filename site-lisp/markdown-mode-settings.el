;;; markdown-mode-settings.el --- Settings for `markdown-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:33>

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


(require 'whitespace)
(require 'dw-functionals)


(autoload 'markdown-mode "markdown-mode.el"
"Majar mode for editing Markdown files" t)

(defun markdown-mode-settings ()
  "Settings for `markdown-mode'."

  (setq whitespace-style
        '(face trailing indentation tabs tab-mark))
  (whitespace-mode 1)
  )

(add-hook 'markdown-mode-hook
          'markdown-mode-settings)

(dw-add-file-mode-pattern-list '(("\\.markdown$" . markdown-mode)
                                 ("\\.md$" . markdown-mode)))


(provide 'markdown-mode-settings)

;;; markdown-mode-settings.el ends here
