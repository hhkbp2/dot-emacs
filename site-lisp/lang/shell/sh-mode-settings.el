;;; sh-mode-settings.el --- Settings for `sh-mode'

;;; Commentary:
;;
;; Settings for `sh-mode',
;; which is mainly for editing shell script.
;;

;;; Code:


(use-package sh-mode
  :defer t
  :config
  (progn
    (add-hook
     'sh-mode-hook
     (lambda()
       ;; set indentation style
       (setq sh-styles-alist
             '(("dw-sh-style"
                (sh-basic-offset . 4)
                (sh-first-lines-indent . 0)
                (sh-indent-after-case . +)
                (sh-indent-after-do . +)
                (sh-indent-after-done . 0)
                (sh-indent-after-else . +)
                (sh-indent-after-function . +)
                (sh-indent-after-if . +)
                (sh-indent-after-loop-construct . +)
                (sh-indent-after-open . +)
                (sh-indent-after-switch . +)
                (sh-indent-comment . t)
                (sh-indent-for-case-alt . ++)
                (sh-indent-for-case-label . +)
                (sh-indent-for-continuation . +)
                (sh-indent-for-do . 0)
                (sh-indent-for-done . 0)
                (sh-indent-for-else . 0)
                (sh-indent-for-fi . 0)
                (sh-indent-for-then . 0))))
       (sh-load-style "dw-sh-style"))))
  )

(provide 'sh-mode-settings)

;;; sh-mode-settings.el ends here
