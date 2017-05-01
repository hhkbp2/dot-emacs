;;; dired-lis-settings.el --- Settings for `dired-lis'

;;; Commentary:

;;; Code:

(use-package dired-lis
  :defer t
  :init
  (progn
    (require 'dired-lis)
    (global-dired-lis-mode))
  :config
  (progn
    (require 'dired-lis-face-settings)
  :bind
  (:map  isearch-mode-map
   ([(control h)] . dired-lis-isearch-up-directory))
  )

(provide 'dired-lis-settings)

;;; dired-lis-settings.el ends here
