;;; undo-tree-settings.el --- Settings for `undo-tree'
;; -*- Emacs-Lisp -*-

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
