;;; rust-mode-settings.el --- Settings for `rust-mode'

;;; Commentary:

;;; Code:


(use-package rust-mode
  :defer t
  :config
  (progn
    (require 'racer)
    (setq racer-cmd (concat (or (getenv "CARGO_HOME")
                                (expand-file-name "~/.cargo")) "/bin/racer")
          racer-rust-src-path (expand-file-name "~/pro/code/rustc-nightly/src/"))
    ;; activate racer when `rust-mode' starts
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    ;; run rustfmt before saving rust buffer
    (add-hook 'rust-mode-hook #'rust-enable-format-on-save)

    ;; key bindings
    (dw-hungry-delete-on-mode-map rust-mode-map)
    (dw-commet-dwin-on-mode-map rust-mode-map)

    (add-hook 'rust-mode-hook
              '(lambda ()
                 ;; turn on `subword-mode' since rust contains camel style names.
                 (subword-mode)
                 ;; turn off `rainbow-delimiters-mode' since it's not smooth in rust-mode
                 (rainbow-delimiters-mode-disable)
                 ;; turn off `electric-indent-mode' since it has some issue with
                 ;; indentation position sometimes
                 (electric-indent-local-mode 0))))
  )

(provide 'rust-mode-settings)

;;; rust-mode-settings.el ends here
