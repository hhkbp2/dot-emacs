;;; cal-china-x-settings.el --- Settings for `cal-china-x'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `cal-china-x':
;; calculating chinese lunar holiday

;;; Code:


(require 'cal-china-x)
;; load face settings
(require 'cal-china-x-face-settings)


(defun cal-china-x-settings ()
  "Settings for `cal-china-x'."
  )


(eval-after-load "cal-china-x"
  '(cal-china-x-settings))


(provide 'cal-china-x-settings)

;;; cal-china-x-settings.el ends here
