;;; scheme-mode-settings.el --- Settings for `scheme-mode'

;;; Commentary:

;;; Code:

(use-package geiser
  :defer t
  :ensure t
  :config
  (progn
    (setq geiser-active-implementations '(guile)))
  )

(use-package scheme
  :defer t
  :config
  (progn
    (require 'geiser))
  )

(provide 'scheme-mode-settings)

;;; scheme-mode-settings.el ends here
