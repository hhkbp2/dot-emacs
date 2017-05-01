;;; linum-face-settings.el --- Face settings for `linum-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(defun linum-face-settings ()
  "Face settings for `linum-mode'."
  (custom-set-faces
   '(linum
     ((((type x))
        (:background "#bbbbbb" :foreground "#555753"
                     :weight normal :slant normal))
      (((class color) (min-colors 256))
        (:background "#bcbcbc" :foreground "#585858"
                     :weight normal :slant normal))
       (((class color) (min-colors 16))
        (:background "white" :foreground "black"
                     :weight normal :slant normal))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"
                     :weight normal :slant normal))
       (t (:background "white" :foreground "black"
                     :weight normal :slant normal))))))


(defun linum-relative-face-settings ()
  "Face settings for `linum-relative'."
  (custom-set-faces
   '(linum-relative-current-line-face
     ((((type x))
        (:background "#e5e5e5" :foreground "#555753"
                     :weight semi-bold :slant normal))
      (((class color) (min-colors 256))
        (:background "#e4e4e4" :foreground "#585858"
                     :weight semi-bold :slant normal))
       (((class color) (min-colors 16))
        (:background "white" :foreground "black"
                     :weight semi-bold :slant normal))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"
                     :weight semi-bold :slant normal))
       (t (:background "white" :foreground "black"
                     :weight semi-bold :slant normal))))))

(eval-after-load 'linum
  '(linum-face-settings))

(eval-after-load 'linum-relative
  '(linum-relative-face-settings))

(provide 'linum-face-settings)

;;; linum-face-settings.el ends here
