;;; linum-settings.el --- Settings for `linum-mode' and its extensions

;;; Commentary:

;;; Code:


(use-package linum+
  :defer t
  :config
  (progn
    ;; (setq linum+-offset t)
    )
  )

(use-package linum-relative
  :defer t
  :config
  (progn
    (linum-relative-on))
  )


(use-package linum
  :defer t
  :init
  (progn
    (require 'dev-base-settings)
    ;; enable `linum-mode' in all modes listed as `usual-mode-hook-list'
    (dolist (mode-hook
             (append usual-mode-hook-list dev-mode-hook-list))
      (add-hook mode-hook
                (lambda ()
                  (linum-mode 1)))))
  :config
  (progn
    (require 'linum+)
    ;;(require 'linum-relative)
    )
  )

(provide 'linum-settings)

;;; linum-settings.el ends here
