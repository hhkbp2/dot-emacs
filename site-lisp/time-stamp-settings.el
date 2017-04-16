;;; time-stamp-settings.el --- Settings for `time-stamp'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `time-stamp'
;; keep time stamp when saving files

;;; Code:


(add-hook 'before-save-hook 'time-stamp)


(defun time-stamp-settings ()
  "Settings for `time-stamp'."

  ;; set time-stamp format
  (setq time-stamp-format "%04y-%02m-%02d %02H:%02M")
  )

(eval-after-load "time-stamp"
  `(time-stamp-settings))


(provide 'time-stamp-settings)

;;; time-stamp-settings.el ends here
