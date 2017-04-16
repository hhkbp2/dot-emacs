;;; erc-face-settings.el --- Face settings for `erc'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun erc-face-settings ()
  "Face settings for `erc'."
  (custom-set-faces
   '(erc-action-face ((t (:weight semi-bold))))
   '(erc-bold-face ((t (:weight bold))))
   '(erc-current-nick-face
     ((t (:foreground "LightSeaGreen" :weight semi-bold))))
   '(erc-dangerous-host-face ((t (:foreground "#ff2f6a"))))
   '(erc-default-face ((t (nil))))
   '(erc-direct-msg-face ((t (:foreground "#ff44cc"))))
   '(erc-error-face
     ((t (:background "darkblue" :foreground "#ff2f6a" :weight semi-bold))))
   '(erc-fool-face ((t (:foreground "dim gray"))))
   '(erc-input-face ((t (:foreground "#009cff"))))
   '(erc-inverse-face
     ((t (:background "Darkgreen" :foreground "Black" :weight semi-bold))))
   '(erc-keyword-face ((t (:foreground "#ff9900" :weight bold))))
   '(erc-nick-default-face ((t (:weight semi-bold))))
   '(erc-nick-msg-face ((t (:foreground "#ff6100" :weight semi-bold))))
   '(erc-notice-face ((t (:foreground "#bb66ff" :weight normal))))
   '(erc-pal-face ((t (:foreground "MediumAquaMarine" :weight bold))))
   '(erc-prompt-face
     ((t (:background "black" :foreground "#ff9900" :weight semi-bold))))
   '(erc-timestamp-face ((t (:foreground "#96ff00" :weight normal))))
   '(erc-underline-face ((t (:underline t)))))
  )


(eval-after-load "erc"
  '(erc-face-settings))


(provide 'erc-face-settings)

;;; erc-face-settings.el ends here
