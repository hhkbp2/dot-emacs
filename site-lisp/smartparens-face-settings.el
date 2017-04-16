;;; smartparens-face-settings.el --- Face settings for `smartparens'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun show-smartparens-face-settings ()
  "Face settings for `show-smartparens-mode'."
  (custom-set-faces
   '(sp-show-pair-match-face
     ((((class color) (min-colors 88))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 16))
       (:background "#bb66ff" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "purple" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "purple" :foreground "white" :weight bold))
      (t (:background "#bb66ff" :foreground "#eeeeee" :weight bold))))
   '(sp-show-pair-mismatch-face
     ((((class color) (min-colors 88))
       (:background "#ff2f6a" :foreground "#232323" :weight bold))
      (((class color) (min-colors 16))
       (:background "#ff2f6a" :foreground "#eeeeee" :weight bold))
      (((class color) (min-colors 8))
       (:background "red" :foreground "white" :weight bold))
      (((type tty) (class mono))
       (:background "red" :foreground "white" :weight bold))
      (t (:background "#ff2f6a" :foreground "#232323" :weight bold))))))

(defun smartparens-face-settings ()
  "Face settings for `smartparens-mode'."
  ;; TODO set these faces:
  ;; sp-pair-overlay-face
  ;; sp-wrap-overlay-face
  ;; sp-wrap-tag-overlay-face
  )

(provide 'smartparens-face-settings)

;;; smartparens-face-settings.el ends here
