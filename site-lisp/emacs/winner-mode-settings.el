;;; winner-mode-settings.el --- Settings for `winner-mode'

;;; Commentary:

;;; Code:


(require 'winner)

(use-package winner
  :defer t
  :config
  (progn
    ;; disable default winner-mode keybindings
    (setq winner-dont-bind-my-keys t)
    (winner-mode t)
    ;; bind `winner-undo' to `Ctrl-x 4'
    (define-key ctl-x-map "4" 'winner-undo)
    ;; bind `winner-redo' to `Ctrl-x 5'
    (define-key ctl-x-map "5" 'winner-redo))
  )

(provide 'winner-mode-settings)

;;; winner-mode-settings.el ends here
