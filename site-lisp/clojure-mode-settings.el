;;; clojure-mode-settings.el --- Settings for the `clojure-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'cider-settings)


(defun clojure-mode-settings ()
  "Settings for `clojure-mode'."

  )

(eval-after-load "clojure-mode"
  `(clojure-mode-settings))

(provide 'clojure-mode-settings)

;;; clojure-mode-settings.el ends here
