;;; color-theme-settings.el --- Setting for `color-theme'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `color-theme'
;; various color themes for emacs

;;; Code:


(require 'color-theme)
(require 'color-theme+)

(defun color-theme-settings ()
  "Settings for `color-theme'."

  (color-theme-initialize)
  )


(eval-after-load "color-theme"
  `(color-theme-settings))


(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
