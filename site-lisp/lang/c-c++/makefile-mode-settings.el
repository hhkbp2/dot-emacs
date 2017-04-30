;;; makefile-mode-settings.el --- Settings for `makefile-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(use-package make-mode
  :ensure t
  :defer t
  :config
  (progn
    (makefile-face-settings)
    (add-hook 'mode-hook
              (lambda()
                (require 'whitespace)
                ;; show whitespace/tab
                (setq whitespace-style '(face tabs tab-mark trailing))
                (whitespace-mode 1)
                )))
  )

(provide 'makefile-mode-settings)

;;; makefile-mode-settings.el ends here
