;;; fiplr-settings.el --- Settings for  `fiplr'

;;; Commentary:

;;; Code:


(use-package fiplr
  :defer t
  :ensure t
  :config
  (progn
    ;; the default value includes these
    ;;(setq fiplr-root-markers '(".git" ".svn"))
    ;; (setq fiplr-ignored-globs '((directories (".git" ".svn"))
    ;;                             (files ("*.jpg" "*.png" "*.zip" "*~"))))
    )
  :bind
  (([(control x) (.)] . fiplr-find-file))
  )

(provide 'fiplr-settings)

;;; fiplr-settings.el ends here
