;;; flyspell-face-settings.el --- Face settings for `flyspell'

;;; Commentary:

;;; Code:


(defun flyspell-face-settings ()
  "Face settings for `flyspell-mode'."
  (custom-set-faces
   '(flyspell-duplicate ((t (:foreground "#fcaf3e" :underline t))))
   '(flyspell-incorrect ((t (:foreground "#cc0000" :strike-through t)))))
  )

(eval-after-load "flyspell"
  '(flyspell-face-settings))

(provide 'flyspell-face-settings)

;;; flyspell-face-settings.el ends here
