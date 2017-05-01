;;; erlang-mode-settings.el --- Settings for `erlang-mode'

;;; Commentary:

;;; Code:


(use-package distel
  :defer t
  :bind
  (:map erlang-shell-mode-map
        ("\C-\M-i" . erl-complete)
        ("\M-?" . erl-complete)
        ("\M-." . erl-find-source-under-point)
        ("\M-," . erl-find-source-unwind)
        ("\M-*" . erl-find-source-unwind))
  )

(use-package erlang
  :defer t
  :mode
  (;; application description file
   ("\\.app$" . erlang-mode)
   ;; release description file
   ("\\.rel$" . erlang-mode)
   ;; release configuration file
   ("\\.config$" . erlang-mode))
  :config
  (progn
    (require 'distel)
    (distel-setup)

    ;; when starting an Erlang shell in Emacs, default in the node name
    (setq inferior-erlang-machine-options '("-sname" "emacs"))

    (require 'dw-functionals)
    (dw-hungry-delete-on-mode-map erlang-mode-map)
    (dw-commet-dwin-on-mode-map erlang-mode-map)

    (add-hook 'erlang-mode-hook
              (lambda ()
                ;; turn on `subword-mode' since erlang uses camel case for
                ;; variable names.
                (subword-mode)

                ;; add Erlang functions to an imenu menu
                (imenu-add-to-menubar "imenu")
                (local-set-key [(control c) (m) (f)] 'mark-erlang-function)
                (local-set-key [(control c) (m) (c)] 'mark-erlang-clause)
                ;; set indentations
                (setq erlang-indent-level 2
                      erlang-indent-guard 4
                      erlang-argument-indent 4))))
  )

(provide 'erlang-mode-settings)

;;; erlang-mode-settings.el ends here
