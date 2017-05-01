;;; scss-mode-settings.el --- Settings for `scss-mode'

;;; Commentary:

;;; Code:

(use-package scss-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; By default `scss-mode' will auto-compile scss file and
    ;; create a responding .css file. Disable it.
    (setq scss-compile-at-save nil)
    )
  )

(provide 'scss-mode-settings)

;;; scss-mode-settings.el ends here
