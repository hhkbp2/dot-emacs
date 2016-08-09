;;; rust-mode-settings.el --- Settings for `rust-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2016 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-08-09 19:55>

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

  (setq racer-cmd (concat (getenv "CARGO_HOME") "/bin/racer")
        racer-rust-src-path (expand-file-name "~/pro/code/rustc-nightly/src/"))
  ;; activate racer when `rust-mode' starts
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  ;; run rustfmt before saving rust buffer
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save)
  )

(eval-after-load "rust-mode"
  `(rust-mode-settings))


(provide 'rust-mode-settings)

;;; rust-mode-settings.el ends here
