;;; wlc-settings.el --- Settings for `wlc'

;;; Commentary:

;;; Code:


(require 'wlc)

(use-package wlc
  :defer t
  :config
  (progn
    ;; enable wlc in `clojure-mode'
    (dolist (mode-hook '(clojure-mode-hook))
      (add-to-list 'wlc/all-features-on-mode-hook-list mode-hook))
    (dolist (mode '(clojure-mode))
      (add-to-list 'wlc/maximum-decoration-mode-list mode))

    ;; turn on wlc
    (wlc/on))
  )

(provide 'wlc-settings)

;;; wlc-settings.el ends here
