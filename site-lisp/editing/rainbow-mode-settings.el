;;; rainbow-mode-settings.el --- Settings for `rainbow-mode'

;;; Commentary:

;;; Code:


(use-package rainbow-mode
  :defer t
  :ensure t
  :config
  (require 'dev-base-settings)

  (setq rainbow-x-colors nil
        rainbow-ansi-colors nil)

  (dolist (mode-hook `(css-mode-hook
                       html-mode-hook
                       ,@dev-mode-hook-list-lisp))
    (add-hook mode-hook 'rainbow-mode))
  )

(provide 'rainbow-mode-settings)

;;; rainbow-mode-settings.el ends here
