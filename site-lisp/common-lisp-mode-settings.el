;;; common-lisp-mode-settings.el --- Settings for `common-lisp-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'slime-settings)
(require 'dw-functionals)


(defun common-lisp-mode-settings ()
  "Settings for `common-lisp-mode'."

  )


(dw-add-file-mode-pattern-list '(("\\.lisp$" . common-lisp-mode)
                                 ("\\.cl$" . common-lisp-mode)))


(provide 'common-lisp-mode-settings)

;;; common-lisp-mode-settings.el ends here
