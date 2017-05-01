;;; desktop-settings.el --- Settings for `desktop'

;;; Commentary:
;;
;; `desktop'
;; keep records of workspace(desktop) in Emacs.

;;; Code:


(use-package desktop
  :defer t
  :init
  (progn
    (desktop-save-mode 1)
    (desktop-auto-save-enable))
  :config
  (progn
    (let ((desktop-file-path "~/.emacs.d/desktop"))
      ;; make sure it exists
      (make-directory desktop-file-path 'NO-ERROR)
      (add-to-list 'desktop-path desktop-file-path))
    (setq desktop-base-file-name "emacs.desktop"))
  )

(provide 'desktop-settings)

;;; desktop-settings.el ends here
