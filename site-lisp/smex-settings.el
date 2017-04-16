;;; smex-settings.el --- Settings for `smex'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'smex)


(defun smex-settings ()
  "Settings for `smex'."

  ;; autoloaded
  ;;(smex-initialize)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'smex-update)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )

(eval-after-load "smex"
  `(smex-settings))


(provide 'smex-settings)
;;; smex-settings.el ends here
