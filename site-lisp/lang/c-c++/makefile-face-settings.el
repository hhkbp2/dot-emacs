;;; makefile-face-settings.el --- Face settings for `makefile-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


;;;###autoload
(defun makefile-face-settings ()
  "Face settings for `makefile-mode'."
  (custom-set-faces
   '(makefile-targets ((t (:foreground "#009cff" :weight semi-bold))))
   '(makefile-space ((t (:background "#ff79d9"))))
   '(makefile-shell
     ((((class color) (min-colors 88))
       (:foreground "wheat" :weight book))
      (((class color) (min-colors 16))
       (:foreground "wheat" :weight book))
      (((class color) (min-colors 8))
       (:foreground "yellow"))
      (((type tty) (class mono))
       (:foreground "yellow"))
      (t (:foreground "wheat" :weight book))))
   '(makefile-makepp-perl ((t (:background "DarkBlue")))))
  )

(provide 'makefile-face-settings)

;;; makefile-face-settings.el ends here
