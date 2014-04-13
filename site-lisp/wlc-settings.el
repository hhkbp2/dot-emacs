;; -*- Emacs-Lisp -*-
;; Settings for `wlc'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-04-13 13:14>

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


(require 'wlc)


(defun wlc-settings ()
  "Settings for `wlc'."

  ;; enable wlc in `clojure-mode'
  (dolist (mode-hook '(clojure-mode-hook))
    (add-to-list 'wlc/all-features-on-mode-hook-list mode-hook))
  (dolist (mode '(clojure-mode))
    (add-to-list 'wlc/maximum-decoration-mode-list mode))

  ;; turn on wlc
  (wlc/on)
  )

(eval-after-load "wlc"
  `(wlc-settings))


(provide 'wlc-settings)
