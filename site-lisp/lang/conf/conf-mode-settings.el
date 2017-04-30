;;; conf-mode-settings.el --- Settings for `conf-mode'

;;; Commentary:

;;; Code:


(use-package conf-mode
  :defer t
  :config
  (progn
    (add-hook 'conf-mode-hook
              (lambda()
                "Settings for `conf-mode'."
                ;; 定制`conf-mode'环境
                ;; 以两个!!开始
                (setq comment-start "!!"))))
  )

(provide 'conf-mode-settings)

;;; conf-mode-settings.el ends here
