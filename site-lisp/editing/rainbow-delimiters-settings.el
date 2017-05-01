;;; rainbow-delimiters-settings.el --- Settings for `rainbow-delimiters'

;;; Commentary:

;;; Code:


(use-package rainbow-delimiters
  :defer t
  :ensure t
  :init
  (progn
    (set-variable 'frame-background-mode 'dark)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  :config
  (progn
    (require 'rainbow-delimiters-face-settings))
  )

(provide 'rainbow-delimiters-settings)

;;; rainbow-delimiters-settings.el ends here
