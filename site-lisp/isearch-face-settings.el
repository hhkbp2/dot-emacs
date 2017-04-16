;;; isearch-face-settings.el --- Face settings for `isearch'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun isearch-face-settings ()
  "Face settings for `isearch'."
  (custom-set-faces
   '(isearch
     ((((class color) (min-colors 88))
       (:background "#fce94f" :foreground "#232323"))
      (((class color) (min-colors 16))
       (:background "#fce94f" :foreground "#232323"))
      (((class color) (min-colors 8))
       (:background "yellow" :foreground "black"))
      (((type tty) (class mono))
       (:background "yellow" :foreground "black"))
      (t (:background "#fce94f" :foreground "#232323"))))
   '(isearch-fail
     ((((class color) (min-colors 88))
       (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))
      (((class color) (min-colors 16))
       (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))
      (((class color) (min-colors 8))
       (:background "red" :foreground "yellow" :weight bold))
      (((type tty) (class mono))
       (:background "red" :foreground "yellow" :weight bold))
      (t (:background "#ff2f6a" :foreground "yellowgreen" :weight bold))))
   '(lazy-highlight ((t (:background "paleturquoise4")))))
  )


(eval-after-load "isearch"
  '(isearch-face-settings))


(provide 'isearch-face-settings)

;;; isearch-face-settings.el ends here
