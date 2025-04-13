;;; line-number-settings.el --- Settings for line numbers

;;; Commentary:

;;; Code:


(require 'dev-base-settings)

;; enable `linum-mode' in all modes listed as `usual-mode-hook-list'
(dolist (mode-hook
         (append usual-mode-hook-list dev-mode-hook-list))
  (add-hook mode-hook
            (lambda ()
              (display-line-numbers-mode 1))))

(provide 'line-number-settings)

;;; line-number-settings.el ends here
