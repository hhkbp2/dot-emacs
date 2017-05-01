;;; clojure-mode-settings.el --- Settings for the `clojure-mode'

;;; Commentary:

;;; Code:

(use-package clojure-mode
  :defer t
  :ensure t
  :config
  (progn
    (require 'cider-settings))
  )

(provide 'clojure-mode-settings)

;;; clojure-mode-settings.el ends here
