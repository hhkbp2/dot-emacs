;;; session-settings.el --- Settings for `session'

;;; Commentary:

;;; Code:

(autoload 'session-initialize "session"
  "Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'." t)

(use-package session
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook 'session-initialize))
  :config
  (progn
    (setq session-save-file "~/.emacs.d/session"
          session-initialize '(session places menus)))
  )

(provide 'session-settings)

;;; session-settings.el ends here
