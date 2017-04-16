;;; conf-mode-settings.el --- Settings for `conf-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;

;;; Code:


(defun conf-mode-settings ()
  "Settings for `conf-mode'."
  ;; 定制`conf-mode'环境
  ;; 以两个!!开始
  (setq comment-start "!!")
  )

(add-hook 'conf-mode-hook
          'conf-mode-settings)


(provide 'conf-mode-settings)

;;; conf-mode-settings.el ends here
