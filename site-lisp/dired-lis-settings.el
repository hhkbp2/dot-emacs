;; -*- Emacs-Lisp -*-

;; Time-stamp: <2011-02-05 21:46>

(require 'dired-lis)
(require 'dired-lis-face-settings)

(global-dired-lis-mode)
(define-key isearch-mode-map (kbd "C-h") 'dired-lis-isearch-up-directory)

(provide 'dired-lis-settings)
