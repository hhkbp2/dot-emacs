;; -*- Emacs-Lisp -*-
;; Settings for `wlc'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-09-15 01:06>

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

  ;; customize like these
  ;; enable wlc in scheme related mode
  ;; (setq wlc/all-features-on-mode-hook-list
  ;;       (append wlc/all-features-on-mode-hook-list
  ;;               '(scheme-mode-hook scheme-interaction-mode-hook)))

  ;; (setq wlc/maximum-decoration-mode-list
  ;;       (append wlc/maximum-decoration-mode-list
  ;;               '(scheme-mode scheme-interaction-mode)))

  ;; turn on wlc
  (wlc/on)
  )

(eval-after-load "wlc"
  `(wlc-settings))


(provide 'wlc-settings)
