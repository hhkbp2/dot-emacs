;; -*- Emacs-Lisp -*-
;; Settings for `grep'.

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-05-18 15:10>

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


(defun neotree-settings ()
  "Settings for `neotree'."

  (setq neo-banner-message "NeoTree")
  (setq neo-smart-open t)
  (setq neo-window-width 34)
  )

(eval-after-load "neotree"
  `(neotree-settings))

(provide 'neotree-settings)
