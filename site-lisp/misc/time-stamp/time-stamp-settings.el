;;; time-stamp-settings.el --- Settings for `time-stamp'

;;; Commentary:
;;
;; `time-stamp'
;; keep time stamp when saving files

;;; Code:

(use-package time-stamp
  :defer t
  :init
  (progn
    (add-hook 'before-save-hook 'time-stamp))
  :config
  (progn
    ;; set time-stamp format
    (setq time-stamp-format "%Y-%02m-%02d %02H:%02M"))
  )

(provide 'time-stamp-settings)

;;; time-stamp-settings.el ends here
