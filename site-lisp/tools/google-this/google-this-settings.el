;;; google-this-settings.el --- Settings for `google-this'

;;; Commentary:

;;; Code:


(use-package google-this
  :defer t
  :ensure t
  :init
  (progn
    ;; turn on `google-this'
    (google-this-mode 1)
    )
  ;; :bind
  ;; (([(control x) (g)] google-this))
  )

(provide 'google-this-settings)

;;; google-this-settings.el ends here
