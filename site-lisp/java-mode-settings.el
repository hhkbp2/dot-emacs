;;; java-mode-settings.el --- Settings for `java-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'cc-mode)


(defun java-mode-settings ()
  "Settings for `java-mode'."

  ;; set indentation style to "java"
  (c-set-style "java")
  ;; enable c-subword-mode
  (if (dw-version->=-23.3)
      (subword-mode 1)
    (c-subword-mode 1))

  (local-set-key [(control c) (c)] 'comment-dwim)
  (local-set-key [(control c) (control c)] 'comment-dwim))


(add-hook 'java-mode-hook
          'java-mode-settings)


(provide 'java-mode-settings)

;;; java-mode-settings ends here
