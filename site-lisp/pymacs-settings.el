;;; pymacs-settings.el --- Settings for `pymacs'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'pymacs)
(require 'python-base-settings)


(defun pymacs-settings ()
  "Settings for `pymacs'."

  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)

  ;; additional module search path
  (setq pymacs-load-path `(,dw-python-dev-dir ,dw-python-path))
  )


(eval-after-load "pymacs"
  `(pymacs-settings))


(provide 'pymacs-settings)

;;; pymacs-settings.el ends here
