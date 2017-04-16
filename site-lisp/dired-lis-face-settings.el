;;; dired-lis-face-settings.el --- Face settings for `dired-lis'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun dired-lis-face-settings ()
  "Face settings for `dired-lis-face-settings'."
  (custom-set-faces
   '(dired-lis-mode-line-face
     ((t (nil)))))
  )


(eval-after-load "dired-lis"
  '(dired-lis-face-settings))


(provide 'dired-lis-face-settings)

;;; dired-lis-face-settings.el ends here
