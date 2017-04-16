;;; thrift-mode-settings.el --- Settings for `thrift-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'thrift)


(defun thrift-mode-settings ()
  (setq thrift-indent-level 4)
  )


(eval-after-load "thrift"
  `(thrift-mode-settings))


(provide 'thrift-mode-settings)

;;; thrift-mode-settings.el ends here
