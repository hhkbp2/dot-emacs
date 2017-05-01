;;; helm-face-settings.el --- Face settings for `helm'

;;; Commentary:

;;; Code:


(defun helm-face-settings ()
  "Face settings for `helm'."
  (custom-set-faces
   '(helm-source-header
     ((((type x)) (:foreground "#bb66ff" :weight bold))
      (((class color) (min-colors 256)) (:foreground "#bb66ff" :weight bold))
      (((class color) (min-colors 16)) (:foreground "purple" :weight bold))
      (((class color) (min-colors 8)) (:foreground "purple" :weight bold))
      (((type tty) (class mono)) (:foreground "purple" :weight bold))
      (t (:foreground "#bb66ff" :weight bold))))
   '(helm-selection
     ((((type x)) (:background "black"))
      (((class color) (min-colors 256)) (:background "black"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (((type tty) (class mono)) (:background "black"))
      (t (:background "black"))))
   )
  )

(provide 'helm-face-settings)

;;; helm-face-settings.el ends here
