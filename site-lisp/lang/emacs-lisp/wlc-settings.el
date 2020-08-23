;;; wlc-settings.el --- Settings for `wlc'

;;; Commentary:

;;; Code:


(require 'wlc)

(use-package wlc
  :defer t
  :config
  (progn
    ;; turn on wlc
    (wlc/on))
  )

(provide 'wlc-settings)

;;; wlc-settings.el ends here
