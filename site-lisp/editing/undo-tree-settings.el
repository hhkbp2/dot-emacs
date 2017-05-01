;;; undo-tree-settings.el --- Settings for `undo-tree'

;;; Commentary:

;;; Code:


(use-package undo-tree
  :defer t
  :ensure t
  :init
  (progn
    (defalias 'redo 'undo-tree-redo)
    (global-undo-tree-mode 1))
  )

(provide 'undo-tree-settings)

;;; undo-tree-settings.el ends here
