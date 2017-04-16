;;; linum-settings.el --- Settings for `linum-mode' and its extensions
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'linum+)
(require 'linum-relative)
;; load face settings
(require 'linum-face-settings)
(require 'dev-base-settings)
(require 'dw-functionals)


(defun linum+-settings ()
  "Settings for `linum'."
  ;; (setq linum+-offset t)
  )


(defun linum-relative-settings ()
  "Settings for `linum-relative'."
  (linum-relative-on))


(defun linum-settings ()
  (dolist (mode-hook
           (append usual-mode-hook-list dev-mode-hook-list))
    (add-hook mode-hook
              (lambda ()
                (linum-mode 1))))
  ;; show line numbers in absolute value
  (linum+-settings)
  ;; alternative, show line numbers
  ;;(linum-relative-settings)
  )


(eval-after-load 'linum
  '(linum-settings))

(provide 'linum-settings)

;;; linum-settings.el ends here
