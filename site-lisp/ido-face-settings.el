;;; ido-face-settings.el --- Face settings for `ido'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun ido-face-settings ()
  "Face settings for `ido'."
  (custom-set-faces
   '(ido-first-match ((t (:weight bold))))
   '(ido-incomplete-regexp ((t (:foreground "Pink" :weight bold))))
   '(ido-indicator
     ((t (:background "red" :foreground "yellow1" :width condensed))))
   '(ido-only-match
     ((((class color) (min-colors 88)) (:foreground "#00c900"))
      (((class color) (min-colors 16)) (:foreground "#00c900"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (((type tty) (class mono)) (:foreground "green"))
      (t (:foreground "#00c900"))))
   '(ido-subdir
     ((((class color) (min-colors 88))
       (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 16))
       (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 8))
       (:foreground "blue" :weight bold))
      (((type tty) (class mono))
       (:foreground "blue" :weight bold))
      (t (:foreground "#9e91ff" :weight bold)))))
  )


(eval-after-load "ido"
  '(ido-face-settings))


(provide 'ido-face-settings)

;;; ido-face-settings.el ends here
