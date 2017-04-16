;;; emacs-lisp-mode-settings.el --- Settings for `emacs-lisp-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun emacs-lisp-mode-settings ()
  "Settings for `emacs-lisp-mode'."

  ;; enter the default elisp debugger: debug, when error happends.
  (setq debug-on-error t)

  ;; pause a few seconds at each break point
  ;; when in edebug `trace' or `continue' mode.
  ;; rather that just 1 second by default
  ;; because probably I need some more time on each break.
  (setq edebug-sit-for-seconds 2)

  ;; enable sexp evaluation.
  (put 'eval-expression 'disable nil)

  ;; key bindings
  ;; 写elisp经常用到`edebug-defun'
  (define-key emacs-lisp-mode-map [(control c) (f)] 'edebug-defun)
  (define-key emacs-lisp-mode-map [(control c) (g)] 'edebug-all-defs)
  )

(emacs-lisp-mode-settings)


(provide 'emacs-lisp-mode-settings)

;;; emacs-lisp-mode-settings.el ends here
