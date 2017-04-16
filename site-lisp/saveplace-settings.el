;;; saveplace-settings.el --- Settings for `saveplace'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `saveplace'
;; save where cursor is when exit

;;; Code:


(require 'saveplace)


(defun saveplace-settings ()
  "Settings for `saveplace'."

  (setq-default save-place t)

  (setq save-place-file "~/.emacs.d/emacs-places")
  )

(eval-after-load "saveplace"
  `(saveplace-settings))


(provide 'saveplace-settings)

;;; saveplace-settings.el ends here
