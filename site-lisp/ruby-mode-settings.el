;;; ruby-mode-settings.el --- Settings for `ruby-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:52>

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



(defun ruby-mode-settings ()
  "Settings for `ruby-mode'."

  )

(eval-after-load "enh-ruby-mode"
  `(ruby-mode-settings))


(defun enh-ruby-mode-settings ()
  "Settings for `enh-ruby-mode'."
  (setq enh-ruby-bounce-deep-indent t
        enh-ruby-hanging-brace-indent-level 2)

  (sp-with-modes '(rhtml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>"))

  (define-key enh-ruby-mode-map "\C-d" 'c-hungry-delete-forward)
  (define-key enh-ruby-mode-map "\177" 'c-hungry-delete-backwards)
  (define-key enh-ruby-mode-map [?\C-c ?\d] 'comment-dwim)
  (define-key enh-ruby-mode-map (kbd "RET") 'newline-and-indent)
  )

(eval-after-load "enh-ruby-mode"
  `(enh-ruby-mode-settings))

(dw-add-file-mode-pattern-list '(("\\.rb$" . enh-ruby-mode)
                                 ("\\.rake$" . enh-ruby-mode)
                                 ("Rakefile$" . enh-ruby-mode)
                                 ("\\.gemspec$" . enh-ruby-mode)
                                 ("\\.ru$" . enh-ruby-mode)
                                 ("Gemfile$" . enh-ruby-mode))
                               t)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'ruby-mode-settings)

;;; ruby-mode-settings.el ends here
