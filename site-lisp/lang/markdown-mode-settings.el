;;; markdown-mode-settings.el --- Settings for `markdown-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'markdown-mode-hook
              (lambda()
                (require 'whitespace)
                (setq whitespace-style
                      '(face trailing indentation tabs tab-mark))
                (whitespace-mode 1))))
  )

(provide 'markdown-mode-settings)

;;; markdown-mode-settings.el ends here
