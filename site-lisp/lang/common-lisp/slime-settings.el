;;; slime-settings.el --- Settings for `slime'

;;; Commentary:

;;; Code:


(use-package slime
  :defer t
  :ensure t
  :config
  (progn
    (require 'wlc)
    ;; 使用sbcl作为common lisp实现
    (setq inferior-lisp-program
          (if (eq system-type 'darwin)
              "/usr/local/bin/sbcl"
            "/usr/bin/sbcl"))
    (slime-setup `(slime-repl slime-fancy slime-fuzzy))
    (setq slime-net-coding-system 'utf-8-unix)
    )
  )

(use-package slime-repl
  :defer t
  :config
  (progn
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode))
  )

(provide 'slime-settings)

;;; slime-settings.el ends here
