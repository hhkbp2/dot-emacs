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
    (require 'helm-descbinds)
    (require 'helm-describe-modes)

    ;; use helm for all
    (helm-mode 1)

    (global-set-key [(control x) (control f)] 'helm-find-files)

    ;; replace `describe-bindings' to `helm-descbinds'
    (helm-descbinds-mode)
    ;; remap `describe-mode' to `helm-describe-modes'
    ;;(global-set-key [remap describe-mode] #'helm-describe-modes)
    )
  )

(use-package helm-ag
  :defer t
  :ensure t)

(use-package helm-swoop
  :defer t
  :ensure t)

(use-package helm-descbinds
  :defer t
  :ensure t)

(use-package helm-describe-modes
  :defer t
  :ensure t)

(provide 'helm-settings)

;;; helm-settings.el ends here
