;; -*- Emacs-Lisp -*-
;; Settings for `ruby-mode'.

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-04-25 18:03>

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
  )

(eval-after-load "enh-ruby-mode"
  `(enh-ruby-mode-settings))

(dw-add-file-mode-pattern-list '(("\\.rb$" . enh-ruby-mode)
                                 ("\\.rake$" . enh-ruby-mode)
                                 ("Rakefile$" . enh-ruby-mode)
                                 ("\\.gemspec$" . enh-ruby-mode)
                                 ("\\.ru$" . enh-ruby-mode)
                                 ("Gemfile$" . enh-ruby-mode))
                               'force-add)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'ruby-mode-settings)
