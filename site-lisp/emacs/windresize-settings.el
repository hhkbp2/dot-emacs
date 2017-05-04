;;; windresize-settings.el --- Settings for `windresize'

;;; Commentary:

;;; Code:


(use-package windresize
  :defer t
  :ensure t
  :bind
  ([(control x) (-)] . windresize)
  )

(provide 'windresize-settings)

;;; windresize-settings.el ends here
