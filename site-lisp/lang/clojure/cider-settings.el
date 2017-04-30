;;; cider-settings.el --- Settings for the `cider'

;;; Commentary:

;;; Code:

(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    ;; enable `eldoc'
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    ;; hide *nrepl-connection* and *nrepl-server* from `switch-to-buffer'
    ;;(setq nrepl-hide-special-buffers)
    ;; remap TAB to only indent
    ;; (setq cider-repl-tab-command 'indent-for-tab-command)
    ;; prevernt the auto-display of the REPL buffer in a separate window
    ;; after connection is established
    (setq cider-repl-pop-to-buffer-on-connect nil)
    ;; stop the error buffer from popping up whie working in buffers other than REPL
    (setq cider-popup-stacktraces nil)
    ;; enable error buffer popping also in the REPL
    (setq cider-repl-popup-stacktraces t)
    ;; to auto-select the error buffer when it's displayed
    (setq cider-auto-select-error-buffer t)
    ;; wrap-stacktrace filters
    (setq cider-stacktrace-default-filters '(tooling dup))
    ;; The REPL buffer name has the format *cider-repl project-name*.
    ;; Change the separator from space to something else by overriding
    ;; `nrepl-buffer-name-separator'.
    (setq nrepl-buffer-name-separator "-")
    ;; The REPL buffer name can also display the port on which the nREPL server
    ;; is running. Buffer name will look like cider-repl project-name:port.
    (setq nrepl-buffer-name-show-port t)
    ;; Make `C-c C-z' switch to the CIDER REPL buffer in the current window
    (setq cider-repl-display-in-current-window t)
    ;; Limit the number of items of each collection the printer will print to 100
    (setq cider-repl-print-length 100) ; the default is nil, no limit
    ;; Prevent `C-c C-k' from prompting to save the file corresponding to
    ;; the buffer being loaded, if it's modified
    (setq cider-prompt-save-file-on-load nil)
    ;; Change the result prefix for REPL evaluation (by default there's no prefix)
    (setq cider-repl-result-prefix ";; => ")
    ;; Change the result prefix for interactive evaluation (by default it's =>)
    (setq cider-interactive-eval-result-prefix ";; => ")
    ;; Normally code you input in the REPL is font-locked with
    ;; `cider-repl-input-face' (after you press RET) and results are font-locked
    ;; with `cider-repl-output-face'. If you want them to be font-locked as
    ;; in clojure-mode use the following:
    (setq cider-repl-use-clojure-font-lock t)
    ;; You can control the `C-c C-z' key behavior of switching to the REPL buffer
    ;; with the `cider-switch-to-repl-command' variable. While the default command
    ;; `cider-switch-to-relevant-repl-buffer' should be an adequate choice for
    ;; most users, `cider-switch-to-current-repl-buffer' offers a simpler
    ;; alternative where CIDER will not attempt to match the correct REPL buffer
    ;; based on underlying project directories
    ;;(setq cider-switch-to-repl-command 'cider-switch-to-current-repl-buffer)
    ;; You can configure known endpoints used by the cider command offered via
    ;; a completing read. This is useful if you have a list of common host/ports
    ;; you want to establish remote nREPL connections to.
    ;; Using an optional label is helpful for identifying each host
    ;; (setq cider-known-endpoints '(("host-a" "10.10.10.1" "7888")
    ;;                               ("host-b" "7888")))

    ;; To make the REPL history wrap around when its end is reached
    (setq cider-repl-wrap-history t)
    ;; To adjust the maximum number of items kept in the REPL history
    (setq cider-repl-history-size 5000)
    ;; To store the REPL history in a file
    (setq cider-repl-history-file "~/.cider-repl-history")

    ;; enabling camelcase support for java
    (add-hook 'cider-repl-mode-hook 'subword-mode)

    ;; make the REPL always pretty-print the result of evaluation
    (cider-repl-toggle-pretty-printing))
  )

(provide 'cider-settings)

;;; cider-settings.el ends here
