;;; go-mode-settings.el --- Settings for `go-mode'

;;; Commentary:

;;; Code:


(require 'dw-functionals)
(require 'go-mode)

(use-package go-mode
  :defer t
  :config
  (progn
    ;; run goimports before saving golang buffer
    ;; non `go-mode' buffer would be intact
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; key bindings
    (dw-hungry-delete-on-mode-map go-mode-map)
    (dw-commet-dwin-on-mode-map go-mode-map)

    ;; Enable `subword-mode' since go is Camel style.
    (add-hook 'go-mode-hook
              '(lambda ()
                 ;; turn on `subword-mode' since golang uses camel case.
                 (subword-mode)
                 ;; keybindings
                 (local-set-key (kbd "M-.") 'godef-jump))))
  )

(provide 'go-mode-settings)

;;; go-mode-settings.el ends here
