;;; osx-settings.el --- Settings for mac osx

;;; Commentary:

;;; Code:


(when (equal system-type `darwin)
  (require 'dash-at-point-settings)
  (require 'exec-path-from-shell)

  (setq exec-path-from-shell-check-startup-files nil)
  ;; set `exec-path' and PATH to the shell path rather than
  ;; the system-wide default
  (exec-path-from-shell-initialize))

(provide 'osx-settings)

;;; osx-settings.el ends here
