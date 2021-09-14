;;; rainbow-delimiters-settings.el --- Settings for `rainbow-delimiters'

;;; Commentary:

;;; Code:


(use-package rainbow-delimiters
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  )

(provide 'rainbow-delimiters-settings)

;;; rainbow-delimiters-settings.el ends here
