;;; ido-settings.el --- Settings for `ido-mode'

;;; Commentary:
;;
;; `ido-mode'
;; open file, find file and switch buffer
;;

;;; Code:


(use-package ido
  :defer t
  :config
  (progn
    (require 'ido-complete-space-or-hyphen)
    (require 'ido-ubiquitous)
    ;; load face settings
    (require 'ido-face-settings)


    (setq ido-save-directory-list-file "~/.emacs.d/ido.last")
    ;; turn on ido mode
    (ido-mode 1)
    ;; toggle ido speed-ups everywhere file and directory names are read.
    (ido-everywhere 1)

    (ido-ubiquitous-mode 1))
  )

(use-package ido-complete-space-or-hyphen
  :defer t
  :ensure t)

(use-package ido-ubiquitous
  :defer t
  :ensure t)

(provide 'ido-settings)

;;; ido-settings.el ends here
