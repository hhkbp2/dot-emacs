;;; python-mode-settings.el --- Settings for `python-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(use-package python-mode
  :defer t
  :ensure t
  :config
  (progn
    (require 'whitespace)

    ;; set tab width
    (setq tab-width 4)

    ;; show whitespace/tab
    (setq whitespace-style
          '(face indentation::tab indentation::space tabs tab-mark trailing))
    (whitespace-mode 1)

    ;; key bindings
    (dw-hungry-delete-on-mode-map python-mode-map)
    (dw-commet-dwin-on-mode-map python-mode-map))
  :bind
  (:map python-mode-map
        (([(return)] . py-newline-and-indent)))
  )

(provide 'python-mode-settings)

;;; python-mode-settings.el ends here
