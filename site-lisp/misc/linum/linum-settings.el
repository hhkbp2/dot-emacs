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
    (require 'linum-face-settings)

    (linum-relative-on))
  )


(use-package linum
  :defec t
  :config
  (progn
    (require 'linum+)
    ;;(require 'linum-relative)
    ;; load face settings
    (require 'linum-face-settings)
    (require 'dev-base-settings)

    (dolist (mode-hook
             (append usual-mode-hook-list dev-mode-hook-list))
      (add-hook mode-hook
                (lambda ()
                  (linum-mode 1))))
    )
  )

(provide 'linum-settings)

;;; linum-settings.el ends here
