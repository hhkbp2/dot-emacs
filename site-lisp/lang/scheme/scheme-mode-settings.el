;;; scheme-mode-settings.el --- Settings for `scheme-mode'

;;; Commentary:

;;; Code:

(use-package geiser
  :defer t
  :ensure t
  :config
  (progn
    (setq geiser-active-implementations '(guile)
          geiser-repl-history-filename "~/.emacs.d/geiser-history"
          geiser-repl-query-on-kill-p nil
          geiser-repl-query-on-exit-p nil)
    (with-eval-after-load 'geiser-guile
      (add-to-list 'geiser-guile-load-path
                   "~/dev/pro/lang/lisp/scheme/sicp/exercise/sicp"))
    ))


(use-package scheme
  :defer t
  :config
  (progn
    (require 'geiser))
  )

(provide 'scheme-mode-settings)

;;; scheme-mode-settings.el ends here
