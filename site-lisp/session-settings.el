;;; session-settings.el --- Settings for `session'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `session'
;; keep records of session in emacs

;;; Code:


(autoload 'session-initialize "session"
"Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'." t)


(add-hook 'after-init-hook 'session-initialize)


(defun session-settings ()
  "Settings for `session'."

  (setq session-save-file "~/.emacs.d/session"
        session-initialize '(session places menus))
  )

(eval-after-load "session"
  `(session-settings))


(provide 'session-settings)

;;; session-settings.el ends here
