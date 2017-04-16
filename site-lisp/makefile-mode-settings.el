;;; makefile-mode-settings.el --- Settings for `makefile-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'make-mode)

;;; whitespace mode
;; show whitespace, tab, empty line, trailing whitespace ...
(require 'whitespace)

;; load face settings
(require 'makefile-face-settings)


(defun makefile-mode-settings()
  "Settings for `makefile-mode'."

  ;; show whitespace/tab
  (setq whitespace-style '(face tabs tab-mark trailing))
  (whitespace-mode 1))


(dolist (mode-hook
         '(makefile-mode-hook))
  (add-hook mode-hook
            'makefile-mode-settings))


(provide 'makefile-mode-settings)

;;; makefile-mode-settings.el ends here
