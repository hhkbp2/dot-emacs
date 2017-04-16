;;; dired-lis-settings.el --- Settings for `dired-lis'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(require 'dired-lis)
(require 'dired-lis-face-settings)

(global-dired-lis-mode)
(define-key isearch-mode-map (kbd "C-h") 'dired-lis-isearch-up-directory)

(provide 'dired-lis-settings)

;;; dired-lis-settings.el ends here
