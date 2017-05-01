;;; dired+-settings.el --- Settings for `dired+'

;;; Commentary:

;;; Code:

(use-package dired+
  :defer t
  :ensure t
  :config
  (progn
    (require 'dired+-face-settings)
    (dired+-face-settings))
  )

(provide 'dired+-settings)

;;; dired+-settings.el ends here
