;;; desktop-settings.el --- Settings for `desktop'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `desktop'
;; keep records of workspace(desktop) in emacs

;;; Code:


(require 'desktop)


(defun desktop-settings ()
  "Settings for `desktop'."

  (let ((desktop-file-path "~/.emacs.d/desktop"))
    ;; make sure it exists
    (make-directory desktop-file-path 'NO-ERROR)
    (add-to-list 'desktop-path desktop-file-path))
  (setq desktop-base-file-name "emacs.desktop")

  (desktop-save-mode 1)
  (desktop-auto-save-enable)
  )


(eval-after-load "desktop"
  `(desktop-settings))


(provide 'desktop-settings)

;;; desktop-settings.el ends here
