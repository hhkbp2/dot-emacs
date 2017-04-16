;;; joxa-mode-settings.el --- Settings for the `joxa-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'joxa-mode)
(require 'dash-at-point)

(defun dw-dash-at-point-for-joxa (&optional edit-docset)
  "Trigger `dash-at-point' for joxa mode with erlang docset."
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol))
         (docset (or edit-docset "erlang")))
    (dash-at-point-run-search
     (replace-regexp-in-string
      "/" ":" (replace-regexp-in-string "-" "_" thing))
     docset)))

(defun joxa-mode-settings ()
  "Settings for `joxa-mode'."

  ;; search dash in erlang docset
  (define-key joxa-mode-map [(control c) (d)] 'dw-dash-at-point-for-joxa)
  )

(eval-after-load "joxa-mode"
  `(joxa-mode-settings))

(provide 'joxa-mode-settings)

;;; joxa-mode-settings.el ends here
