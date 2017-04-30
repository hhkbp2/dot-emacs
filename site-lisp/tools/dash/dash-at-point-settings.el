;;; dash-at-point-settings.el --- Settings for `dash-at-point'

;;; Commentary:

;;; Code:


(use-package dash-at-point
  :ensure t
  :commands dash-at-point
  :bind (([(control c) (d)] . dash-at-point))
  )

(provide 'dash-at-point-settings)

;;; dash-at-point-settings.el ends here
