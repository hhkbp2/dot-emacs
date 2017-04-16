;;; wlc-settings.el --- Settings for `wlc'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'wlc)


(defun wlc-settings ()
  "Settings for `wlc'."

  ;; enable wlc in `clojure-mode'
  (dolist (mode-hook '(clojure-mode-hook))
    (add-to-list 'wlc/all-features-on-mode-hook-list mode-hook))
  (dolist (mode '(clojure-mode))
    (add-to-list 'wlc/maximum-decoration-mode-list mode))

  ;; turn on wlc
  (wlc/on)
  )

(eval-after-load "wlc"
  `(wlc-settings))


(provide 'wlc-settings)

;;; wlc-settings.el ends here
