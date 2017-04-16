;;; sh-mode-settings.el --- Settings for `sh-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; Settings for `sh-mode',
;; which is mainly for editing shell script.
;;

;;; Code:



(defun sh-mode-settings ()
  "Settings for `sh-mode'."

  ;; TODO add settings

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
  (sh-load-style "dw-sh-style")
  )

;; (eval-after-load "sh-script"
;;   `(sh-mode-settings))

(dolist (mode-hook
         '(sh-mode-hook))
  (add-hook mode-hook 'sh-mode-settings))


(provide 'sh-mode-settings)

;;; sh-mode-settings.el ends here
