;;; common-lisp-mode-settings.el --- Settings for `common-lisp-mode'

;;; Commentary:

;;; Code:


(use-package lisp-mode
  :mode (("\\.lisp$" . common-lisp-mode)
         ("\\.cl$" . common-lisp-mode))
  :config
  (progn
    (require 'slime-settings))
  )

(provide 'common-lisp-mode-settings)

;;; common-lisp-mode-settings.el ends here
