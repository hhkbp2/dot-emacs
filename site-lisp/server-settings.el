;;; server-settings.el --- Settings for `server'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'server)


(defun server-settings ()
  "Settings for `server'."

  (setq server-socket-dir "~/.emacs.d/server")
  )


(eval-after-load "server"
  `(server-settings))


(provide 'server-settings)

;;; server-settings.el ends here
