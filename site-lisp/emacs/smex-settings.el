;;; smex-settings.el --- Settings for `smex'

;;; Commentary:

;;; Code:


(use-package smex
  :defer t
  :bind
  (([(meta x)] . smex)
   ([(meta X)] . smex-major-mode-commands)
   ([(control c)(meta x)] . smex-update)
   ([(control c)(control c)(meta x)] . execute-extended-command))
  )


(provide 'smex-settings)
;;; smex-settings.el ends here
