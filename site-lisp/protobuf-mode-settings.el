;;; protobuf-mode-settings.el --- Settings for `protobuf-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


(defun protobuf-mode-settings ()
  )


(eval-after-load "protobuf-mode"
  `(protobuf-mode-settings))


(provide 'protobuf-mode-settings)

;;; protobuf-mode-settings.el ends here
