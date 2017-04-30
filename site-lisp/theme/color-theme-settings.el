;;; color-theme-settings.el --- Setting for `color-theme'

;;; Commentary:
;;
;; `color-theme'
;; various color themes for emacs

;;; Code:


(use-package color-theme
  :defer t
  :config
  (progn
    (require 'color-theme+)
    (color-theme-initialize))
  )

(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
