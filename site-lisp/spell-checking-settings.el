;;; spell-checking-settings.el --- Settings for spell checking
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(require 'dev-base-settings)
(require 'flyspell-face-settings)


(defun spell-checking-settings ()
  "Settings for spell checking."

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

(spell-checking-settings)
;;(eval-after-load "flyspell"
;;  `(spell-checking-settings))


(provide 'spell-checking-settings)

;;; spell-checking-settings.el ends here
