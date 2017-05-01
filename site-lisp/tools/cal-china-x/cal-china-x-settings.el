;;; cal-china-x-settings.el --- Settings for `cal-china-x'

;;; Commentary:
;;
;; `cal-china-x':
;; calculating chinese lunar holiday

;;; Code:


(use-package cal-china-x
  :defer t
  :ensure t
  :config
  (progn
    (require 'cal-china-x-face-settings))
  )

(provide 'cal-china-x-settings)

;;; cal-china-x-settings.el ends here
