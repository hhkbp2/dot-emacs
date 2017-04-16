;;; windresize-settings.el --- Settings for `windresize'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'windresize)


(defun windresize-settings ()
  "Settings for `windresize'."

  (global-set-key [(control x) (-)] 'windresize)
  )

(eval-after-load "windresize"
  `(windresize-settings))


(provide 'windresize-settings)

;;; windresize-settings.el ends here
