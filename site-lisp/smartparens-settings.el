;;; smartparens-settings.el --- Settings for `smartparens'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'smartparens-config)
(require 'smartparens-face-settings)


(defun smartparens-settings()
  "Settings for `smartparens'."
  (smartparens-face-settings)
  (show-smartparens-face-settings)
  )

(eval-after-load "smartparens"
  `(smartparens-settings))

(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; TODO add smartparens settings for erlang mode

(provide 'smartparens-settings)

;;; smartparens-settings.el ends here
