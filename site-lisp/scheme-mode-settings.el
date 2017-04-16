;;; scheme-mode-settings.el --- Settings for `scheme-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


;;(require 'xscheme)
(require 'scheme)
(require 'cl)
(require 'geiser)


(defun geiser-settings ()
  "Settings for `geiser'."

  (setq geiser-active-implementations '(guile)))
(geiser-settings)


(defun scheme-mode-settings ()
  "Settings for `scheme-mode'."

  ;;(make-local-variable 'scheme-indent-function)
  )

(eval-after-load "scheme-mode"
  `(scheme-mode-settings))


(provide 'scheme-mode-settings)

;;; scheme-mode-settings.el ends here
