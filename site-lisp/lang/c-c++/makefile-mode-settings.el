;;; makefile-mode-settings.el --- Settings for `makefile-mode'

;;; Commentary:

;;; Code:


(use-package make-mode
  :defer t
  :config
  (progn
    (require 'makefile-face-settings)

    (add-hook 'makefile-mode-hook
              (lambda()
                (require 'whitespace)
                ;; show whitespace/tab
                (setq whitespace-style '(face tabs tab-mark trailing))
                (whitespace-mode 1)
                )))
  )

(provide 'makefile-mode-settings)

;;; makefile-mode-settings.el ends here
