;;; neotree-settings.el --- Settings for `neotree'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun neotree-settings ()
  "Settings for `neotree'."

  (setq neo-banner-message "NeoTree")
  (setq neo-smart-open t)
  (setq neo-window-width 34)
  )

(eval-after-load "neotree"
  `(neotree-settings))

(provide 'neotree-settings)

;;; neotree-settings.el ends here
