;;; highlight-symbol-settings.el --- Settings for `highlight-symbol'

;;; Commentary:

;;; Code:


(require 'highlight-symbol)

(defun highlight-symbol-mode-on ()
  "Turn on function `highlight-symbol-mode'."
  (highlight-symbol-mode 1))

(defun highlight-symbol-mode-off ()
  "Turn off function `highlight-symbol-mode'."
  (highlight-symbol-mode -1))

(use-package highlight-symbol
  :defer t
  :ensure t
  :config
  (progn
    (setq highlight-symbol-idle-delay 0.5))
  :bind
  (([(control c) (meta h)] . highlight-symbol-at-point)
   ([(control c) (meta r)] . highlight-symbol-remove-all)
   ([(control c) (meta n)] . highlight-symbol-next)
   ([(control c) (meta p)] . highlight-symbol-prev))
  )

(provide 'highlight-symbol-settings)

;;; highlight-symbol-settings.el ends here
