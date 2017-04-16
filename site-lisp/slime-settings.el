;;; slime-settings.el --- Settings for `slime'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'slime-autoloads)
(require 'wlc)


(defun slime-settings ()
  "Settings for `slime'."

  ;; 使用sbcl作为common lisp实现
  (setq inferior-lisp-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/sbcl"
          "/usr/bin/sbcl"))
  (slime-setup `(slime-repl slime-fancy slime-fuzzy))
  (setq slime-net-coding-system 'utf-8-unix)
  )

(defun slime-repl-settings ()
  "Settings for `slime-repl-mode'."

  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
  )

(eval-after-load "slime"
  `(slime-settings))

(eval-after-load "slime-repl"
  `(slime-repl-settings))


(provide 'slime-settings)

;;; slime-settings.el ends here
