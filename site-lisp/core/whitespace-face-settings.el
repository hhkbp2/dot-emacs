;;; whitespace-face-settings.el --- Face settings for `whitespace'

;;; Commentary:

;;; Code:


(defun whitespace-face-settings ()
  "Face settings for `whitespace'."
  (custom-set-faces
   '(whitespace-space
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-space-before-tab
     ((t (:background "DarkOrange" :foreground "#ff79d9"))))
   '(whitespace-space-after-tab
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-hspace
     ((t (:background "gray22" :foreground "#ff79d9"))))
   '(whitespace-tab
     ((((type x)) (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 256))
       (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 16))
       (:background "gray30" :foreground "#ff79d9"))
      (((class color) (min-colors 8))
       (:background "white" :foreground "red"))
      (((type tty) (class mono))
       (:background "white" :foreground "red"))
      (t (:background "gray30" :foreground "#ff79d9"))))
   '(whitespace-newline
     ((t (:foreground "darkgray" :weight normal))))
   '(whitespace-trailing
     ((((type x)) (:background "gray30"))
      (((class color) (min-colors 256)) (:background "gray30"))
      (((class color) (min-colors 16)) (:background "gray30"))
      (((class color) (min-colors 8)) (:background "gray30"))
      (t (:background "gray30"))))
   '(whitespace-line
     ((t (:background "gray22" :foreground "violet"))))
   '(whitespace-indentation
     ((((type x)) (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 256))
       (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 16))
       (:background "gray40" :foreground "#ff79d9"))
      (((class color) (min-colors 8))
       (:background "white" :foreground "red"))
      (t (:background "gray40" :foreground "#ff79d9"))))
   '(whitespace-empty
     ((t (:background "gray22" :foreground "#ff79d9"))))
   )
  )

(provide 'whitespace-face-settings)

;;; whitespace-face-settings.el ends here
