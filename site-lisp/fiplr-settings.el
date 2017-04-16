;;; fiplr-settings.el --- Settings for  `fiplr'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'fiplr)

(defun fiplr-settings ()
  "Settings for `fiplr'."

  ;; the default value includes these
  ;;(setq fiplr-root-markers '(".git" ".svn"))
  ;; (setq fiplr-ignored-globs '((directories (".git" ".svn"))
  ;;                             (files ("*.jpg" "*.png" "*.zip" "*~"))))

  ;; key bindings
  (global-set-key [(control x) (.)] 'fiplr-find-file))


(eval-after-load "fiplr"
  `(fiplr-settings))

(provide 'fiplr-settings)

;;; fiplr-settings.el ends here
