;; -*- Emacs-Lisp -*-
;; Settings for `tree-buffer'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-04 12:24>

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


(require 'tree-buffer)


(defadvice tree-buffer-create (after tree-buffer-keybindings)
  "Customize the keybindings of `tree-buffer'."
  ;; we need to do this in advice rather than call `define-key'
  ;; on `tree-buffer-key-map' globally, because this variable
  ;; `tree-buffer-key-map' is created as a key map when
  ;; `tree-buffer-create' is called.
  ;; key bindings
  ;; (define-key tree-buffer-key-map [home] 'beginning-of-buffer)
  ;; (define-key tree-buffer-key-map [end] 'end-of-buffer)
  ;; (define-key tree-buffer-key-map [(control s)] 'tree-buffer-incremental-node-search)
  )


(defun tree-buffer-settings ()
  "Settings for `tree-buffer'."

  ;; active advices
  ;;(ad-activate 'tree-buffer-create)
  )


(eval-after-load "tree-buffer"
  `(tree-buffer-settings))


(provide 'tree-buffer-settings)