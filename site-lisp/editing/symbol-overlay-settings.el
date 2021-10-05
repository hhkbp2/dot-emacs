;;; symbol-overlay-settings.el --- Settings for `symbol-overlay'

;;; Commentary:

;;; Code:

(require 'symbol-overlay)

(defun symbol-overlay-mode-enable ()
  "Enable `symbol-overlay-mode'."
  (interactive)
  (symbol-overlay-mode 1))

(use-package symbol-overlay
  :defer t
  :ensure t
  :bind
  (([(control c) (meta h)] . symbol-overlay-put)
   ([(control c) (meta r)] . symbol-overlay-remove-all)
   ([(control c) (meta n)] . symbol-overlay-jump-next)
   ([(control c) (meta p)] . symbol-overlay-jump-prev))
  )

(provide 'symbol-overlay-settings)

;;; symbol-overlay-settings.el ends here
