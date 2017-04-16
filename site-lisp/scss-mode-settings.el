;;; scss-mode-settings.el --- Settings for `scss-mode'
;; -*- Emacs-Lisp -*-;

;;; Commentary:

;;; Code:

(defun scss-mode-settings()
  "Settings for `scss-mode'."

  ;; by default `scss-mode' will auto-compile scss file and create a responding .css file.
  ;; disable it
  (setq scss-compile-at-save nil)
  )

(eval-after-load "scss-mode"
  '(scss-mode-settings))

(provide 'scss-mode-settings)

;;; scss-mode-settings.el ends here
