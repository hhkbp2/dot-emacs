;;; neotree-settings.el --- Settings for `neotree'

;;; Commentary:

;;; Code:


(use-package neotree
  :defer t
  :config
  (progn
    (setq neo-banner-message "NeoTree")
    (setq neo-smart-open t)
    (setq neo-window-width 34))
  )

(provide 'neotree-settings)

;;; neotree-settings.el ends here
