;;; joxa-mode-settings.el --- Settings for the `joxa-mode'

;;; Commentary:

;;; Code:

(defun dw-dash-at-point-for-joxa (&optional edit-docset)
  "Trigger `dash-at-point' for joxa mode with erlang docset."
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol))
         (docset (or edit-docset "erlang")))
    (dash-at-point-run-search
     (replace-regexp-in-string
      "/" ":" (replace-regexp-in-string "-" "_" thing))
     docset)))

(use-package joxa-mode
  :defer t
  :bind
  (:map joxa-mode-map
        (([(control c) (d)] . dw-dash-at-point-for-joxa)))
  )

(provide 'joxa-mode-settings)

;;; joxa-mode-settings.el ends here
