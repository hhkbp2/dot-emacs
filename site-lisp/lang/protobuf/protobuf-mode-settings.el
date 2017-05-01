;;; protobuf-mode-settings.el --- Settings for `protobuf-mode'

;;; Commentary:

;;; Code:


(use-package protobuf-mode
  :defer t
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode))
  )

(provide 'protobuf-mode-settings)

;;; protobuf-mode-settings.el ends here
