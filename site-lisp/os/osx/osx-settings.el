;;; osx-settings.el --- Settings for mac osx

;;; Commentary:

;;; Code:

(defun osx-settings ()
  "Settings for mac osx."

  ;; set `exec-path' and PATH to the shell path rather than
  ;; the system-wide default
  (exec-path-from-shell-initialize))

(when (equal system-type `darwin)
  (require 'exec-path-from-shell)
  (require 'dash-at-point-settings)
  (osx-settings))

(provide 'mac-settings)

;;; osx-settings.el ends here
