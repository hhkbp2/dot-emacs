;;; python-ropemacs-settings.el --- Settings for `python-ropemacs'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require' pymacs-settings)


(defun python-ropemacs-settings ()
  "Settings for `python-ropemacs'."

  ;; TODO what to set?
  )


(defun python-ropemacs-load()
  "Load ropemacs library."

;;  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-global-prefix "C-c p")

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os" "sys"))
  (ropemacs-mode t))


(provide 'python-ropemacs-settings)

;;; python-ropemacs-settings.el ends here
