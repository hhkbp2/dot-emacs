;;; undo-tree-settings.el --- Settings for `undo-tree'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 15:11>

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


(require 'undo-tree)


(defun undo-tree-settings ()
  "Settings for `redo+'."
  (defalias 'redo 'undo-tree-redo)
  (global-undo-tree-mode 1)
  )

(eval-after-load "undo-tree"
  `(undo-tree-settings))


(provide 'undo-tree-settings)

;;; undo-tree-settings.el ends here
