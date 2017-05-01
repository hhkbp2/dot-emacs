;;; helm-settings.el --- Settings for `helm'

;;; Commentary:

;;; Code:

(use-package helm
  :defer t
  :ensure t
  :config
  (progn
    (require 'helm-config)
    (require 'helm-swoop)
    (require 'helm-files)
    (require 'helm-face-settings)
    (require 'helm-descbinds)
    (require 'helm-describe-modes)

    (helm-face-settings)

    ;; use helm for all
    (helm-mode 1)

    (global-set-key [(control x) (control f)] 'helm-find-files)

    ;; replace `describe-bindings' to `helm-descbinds'
    (helm-descbinds-mode)
    ;; remap `describe-mode' to `helm-describe-modes'
    ;;(global-set-key [remap describe-mode] #'helm-describe-modes)
    )
  )

(provide 'helm-settings)

;;; helm-settings.el ends here
