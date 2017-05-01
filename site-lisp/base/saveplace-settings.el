;;; saveplace-settings.el --- Settings for `saveplace'

;;; Commentary:
;;
;; `saveplace'
;; save where cursor is when exit

;;; Code:


(use-package saveplace
  :defer t
  :init
  (progn
    (save-place-mode 1))
  :config
  (progn
    (setq save-place-file "~/.emacs.d/emacs-places"))
  )

(provide 'saveplace-settings)

;;; saveplace-settings.el ends here
