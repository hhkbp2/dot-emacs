;;; calendar-face-settings.el --- Face settings for `calendar'.
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun calendar-face-settings ()
  "Face settings for `calendar'."
  (custom-set-faces
   '(calendar-today
     ((((class color) (min-colors 88))
       (:background "black" :foreground "#ff9900" :underline t))
      (((class color) (min-colors 16))
       (:background "black" :foreground "#ff9900" :underline t))
      (((class color) (min-colors 8))
       (:background "white" :foreground "yellow"))
      (((type tty) (class mono))
       (:background "white" :foreground "yellow"))
      (t (:background "black" :foreground "#ff9900"))))
   '(diary ((t (:foreground "yellow1"))))
   '(holiday
     ((((class color) (min-colors 88))
       (:background "gray30" :foreground "#acc900" :slant italic))
      (((class color) (min-colors 16))
       (:background "gray30" :foreground "#acc900" :slant italic))
      (((class color) (min-colors 8))
       (:background "white" :foreground "green" :slant italic))
      (((type tty) (class mono))
       (:background "white" :foreground "green" :slant italic))
      (t (:background "gray30" :foreground "#acc900" :slant italic)))))
  )


(eval-after-load "calendar"
  '(calendar-face-settings))


(provide 'calendar-face-settings)

;;; calendar-face-settings.el ends here
