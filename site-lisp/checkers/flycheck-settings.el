;;; flycheck-settings.el --- Settings for `flycheck'

;;; Commentary:

;;; Code:


(defun flycheck-4-elisp ()
  "Flycheck settings for `emacs-lisp-mode'."
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(defun flycheck-4-erlang ()
  "Flycheck settings for `erlang-mode'."
  ;; TODO customize
  ;; flycheck-erlang-include-path
  ;; flycheck-erlang-library-path
  (add-hook 'erlang-mode-hook 'flycheck-mode)
  )

(defun flycheck-4-elixir ()
  "Flycheck settings for `elixir-mode'."

  ;; add flycheck checker for elixir
  (flycheck-define-checker elixir-mix
    "An Elixir syntax checker using the Elixir interpreter.
Refer to `https://github.com/ananthakumaran/dotfiles/.emacs.d/init-elixir.el'."
    :command ("mix" "compile" source)
    :error-patterns ((error line-start
                            "** (" (zero-or-more not-newline) ") "
                            (zero-or-more not-newline) ":" line ": " (message)
                            line-end)
                     (warning line-start
                              (one-or-more (not (syntax whitespace))) ":"
                              line ": "
                              (message)
                              line-end))
    :modes elixir-mode)
  (add-to-list 'flycheck-checkers 'elixir-mix)
  (add-hook 'elixir-mode-hook 'flycheck-mode))


(defun flycheck-4-go ()
  "Flycheck settings for `go-mode'."
  (require 'go-flycheck)
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package flycheck
  :defer t
  :config
  (progn
    ;; check syntax when
    ;; 1. `flycheck-mode' is enabled
    ;; 2. the buffer is save
    (setq flycheck-check-syntax-automatically '(mode-enabled save))

    ;; enable flycheck in specified modes
    (dolist (func '(flycheck-4-elisp
                    flycheck-4-erlang
                    flycheck-4-elixir
                    flycheck-4-go
                    ))
      (funcall func)))
  )

(provide 'flycheck-settings)

;;; flycheck-settings.el ends here
