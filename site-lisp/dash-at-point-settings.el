;;; dash-at-point-settings.el --- Settings for `dash-at-point'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(require 'dash-at-point)

(defun dash-at-point-settings ()
  "Settings for `dash-at-point'."

  (global-set-key [(control c) (d)] 'dash-at-point)
  )


(eval-after-load "dash-at-point"
  `(dash-at-point-settings))


(provide 'dash-at-point-settings)

;;; dash-at-point-settings.el ends here
