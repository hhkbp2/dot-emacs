;;; spell-checking-settings.el --- Settings for spell checking

;;; Commentary:

;;; Code:


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(use-package flyspell
  :defer t
  :config
  (progn
    (require 'dev-base-settings)
    (require 'flyspell-face-settings)

    ;; use apsell as ispell backend
    (setq-default ispell-program-name "aspell")
    ;; use American English as ispell default dictionary
    (ispell-change-dictionary "american" t)

    ;; (global-flyspell-mode)

    (dolist (hook
             `(;;text-mode-hook
               ;;,@dev-mode-hook-list
               ))
      (add-hook hook 'flyspell-mode))
    )
  )

(provide 'spell-checking-settings)

;;; spell-checking-settings.el ends here
