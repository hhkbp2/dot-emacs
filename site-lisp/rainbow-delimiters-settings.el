;;; rainbow-delimiters-settings.el --- Settings for `rainbow-delimiters'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'rainbow-delimiters)
(require 'rainbow-delimiters-face-settings)


(defun rainbow-delimiters-settings ()
  "Settings for `rainbow-delimiters'."

  )

(eval-after-load "rainbow-delimiters"
  `(rainbow-delimiters-settings))


(defun enable-rainbow-delimiters ()
  (set-variable 'frame-background-mode 'dark)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(enable-rainbow-delimiters)


(provide 'rainbow-delimiters-settings)

;;; rainbow-delimiters-settings.el ends here
