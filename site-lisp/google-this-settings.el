;;; google-this-settings.el --- Settings for `google-this'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'google-this)


(defun google-this-settings ()
  "Settings for `google-this'."

  ;;(global-set-key (kbd "C-x g") 'google-this-mode-submap)
  )


(eval-after-load "google-this"
  `(google-this-settings))

;; turn on `google-this'
(google-this-mode 1)


(provide 'google-this-settings)

;;; google-this-settings.el ends here
