;;; thrift-mode-settings.el --- Settings for `thrift-mode'

;;; Commentary:

;;; Code:

(use-package thrift
  :defer t
  :ensure t
  :mode ("\\.thrift\\'" . thrift-mode)
  :config
  (progn
    (setq thrift-indent-level 4))
  )

(provide 'thrift-mode-settings)

;;; thrift-mode-settings.el ends here
