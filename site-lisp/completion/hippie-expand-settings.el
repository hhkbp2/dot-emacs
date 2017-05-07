;;; hippie-expand-settings.el --- Settings for `hippie-expand'

;;; Commentary:

;;; Code:

(use-package hippie-exp
  :defer t
  :config
  (progn
    (require 'ahei-misc)
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-whole-kill))
    (am-add-hooks
     `(emacs-lisp-mode-hook lisp-interaction-mode-hook)
     '(lambda ()
        (make-local-variable 'hippie-expand-try-functions-list)
        (setq hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-visible
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-expand-whole-kill))))
    )
  :bind
  (([(meta /)] . hippie-expand))
  )

(provide 'hippie-expand-settings)

;;; hippie-expand-settings.el ends here
