;;; mac-settings.el --- Settings for mac os
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'exec-path-from-shell)
(require 'dash-at-point-settings)


(defun mac-settings ()
  "Settings for mac os."

  ;; set `exec-path' and PATH to the shell path rather than
  ;; the system-wide default
  (exec-path-from-shell-initialize))


(provide 'mac-settings)

;;; mac-settings.el ends here
