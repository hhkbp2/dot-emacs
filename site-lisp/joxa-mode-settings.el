;;; joxa-mode-settings.el --- Settings for the `joxa-mode'
;; -*- Emacs-Lisp -*-

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 12:00>

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


(require 'joxa-mode)
(require 'dash-at-point)

(defun dw-dash-at-point-for-joxa (&optional edit-docset)
  "Trigger `dash-at-point' for joxa mode with erlang docset."
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol))
         (docset (or edit-docset "erlang")))
    (dash-at-point-run-search
     (replace-regexp-in-string
      "/" ":" (replace-regexp-in-string "-" "_" thing))
     docset)))

(defun joxa-mode-settings ()
  "Settings for `joxa-mode'."

  ;; search dash in erlang docset
  (define-key joxa-mode-map [(control c) (d)] 'dw-dash-at-point-for-joxa)
  )

(eval-after-load "joxa-mode"
  `(joxa-mode-settings))

(provide 'joxa-mode-settings)

;;; joxa-mode-settings.el ends here
