;;; cal-china-x-face-settings.el --- Face settings for `cal-china-x'

;;; Commentary:

;;; Code:


(defun cal-china-x-face-settings ()
  "Face settings for `cal-china-x'."
  (custom-set-faces
   '(cal-china-x-general-holiday-face ((t (:background "green"))))
   '(cal-china-x-important-holiday-face ((t (:background "red")))))
  )

(eval-after-load "cal-china-x"
  '(cal-china-x-face-settings))

(provide 'cal-china-x-face-settings)

;;; cal-china-x-face-settings.el ends here
