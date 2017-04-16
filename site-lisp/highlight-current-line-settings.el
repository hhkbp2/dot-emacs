;;; highlight-current-line-settings.el --- Settings for `highlight-current-line'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


;; load `highlight-current-line', if it's not loaded yet
(require 'highlight-current-line)

(require 'dev-base-settings)


(defun highlight-current-line-settings()
  "Settings for `highlight-current-line'."

  ;; enable `highlight-current-line' for emacs globally
  ;; seems not working by only setting this value
  (setq highlight-current-line-globally t)

  ;; enable highlight on current line
  (highlight-current-line-minor-mode 1)

  ;; The comment of `highlight-current-line-whole-line' is wrong by
  ;; mixing up the value and its corresponding effect.
  ;; Correct comment should be:
  ;; nil: highlight the line up to the end of line
  ;; t: highlight the whole line up to window-border
  (highlight-current-line-whole-line-on t))


;; apply settings to almost all modes
(dolist (mode-hook
         `(custom-mode-hook find-file-hook apropos-mode-hook ibuffer-mode-hook
                            dired-mode-hook browse-kill-ring-mode-hook
                            completion-list-mode-hook package-menu-mode-hook
                            data-debug-hook log-view-mode-hook chart-mode-hook

                            help-mode-hook Info-mode-hook
                            Man-mode-hook

                            text-mode-hook diff-mode-hook
                            color-theme-mode-hook hs-hide-hook

                            html-mode-hook

                            gdb-mode-hook compilation-mode-hook
                            semantic-symref-results-mode-hook

                            ,@dev-mode-hook-list
                            ))

  (add-hook mode-hook 'highlight-current-line-settings))


(provide 'highlight-current-line-settings)

;;; highlight-current-line-settings.el ends here
