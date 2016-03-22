;;; rust-mode-settings.el --- Settings for `rust-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2016 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 14:53>

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


(require 'rust-mode)


(defun rust-mode-settings ()
  "Settings for `rust-mode'."

  (setq racer-cmd (expand-file-name "~/local/bin/racer")
        racer-rust-src-path (expand-file-name "~/pro/code/rustc-1.6.0/src/"))
  ;; activate racer when `rust-mode' starts
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  ;;
  )

(eval-after-load "rust-mode"
  `(rust-mode-settings))


(provide 'rust-mode-settings)

;;; rust-mode-settings.el ends here
