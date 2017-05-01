;;; smartparens-settings.el --- Settings for `smartparens'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(use-package smartparens
  :defer t
  :ensure t
  :init
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))
  :config
  (progn
    (require 'smartparens-face-settings))
  )

(provide 'smartparens-settings)

;;; smartparens-settings.el ends here
