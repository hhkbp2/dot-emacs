;;; server-settings.el --- Settings for `server'

;;; Commentary:

;;; Code:


(use-package server
  :defer t
  :config
  (progn
    (setq server-socket-dir "~/.emacs.d/server"))
  )

(provide 'server-settings)

;;; server-settings.el ends here
