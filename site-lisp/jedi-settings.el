;;; jedi-settings.el --- Settings for `jedi'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'jedi)


(defun jedi-settings ()
  "Settings for `jedi'."

  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  )

(jedi-settings)


(provide 'jedi-settings)

;;; jedi-settings.el ends here
