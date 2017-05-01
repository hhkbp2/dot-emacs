;;; gtags-settings.el --- Settings for `gtags-mode'

;;; Commentary:

;;; Code:


(defadvice gtags-select-mode (after gtags-select-mode-linum-on)
  "Settings for `gtags-select-mode'."
  (require 'linum-settings)
  (linum-mode 1)
  ;; key-bindings
  (define-key gtags-mode-map [(meta \,)] 'gtags-pop-stack)
  (define-key gtags-mode-map [(meta .)] 'gtags-select-tag))

(use-package gtags
  :defer t
  :ensure t
  :init
  (progn
    (require 'dev-base-settings)
    (dolist (mode-hook dev-mode-hook-list-static)
      (add-hook mode-hook
                '(lambda ()
                   (gtags-mode 1)
                   (gtags-settings)))))
  :config
  (progn
    (let ((dir (expand-directory-name "~/pro")))
      (if dir
          (setq gtags-rootdir dir)))

    ;; set code path display style in `gtags-select-mode'
    ;; alternative values: root, relative, absolute
    (setq gtags-path-style 'root)

    ;; activate advices
    (ad-activate 'gtags-select-mode)
    )
  :bind
  (:map gtags-mode-map
        ([(meta \,)] . gtags-pop-stack)
        ([(meta .)] .  gtags-find-tag-from-here))
  )

(provide 'gtags-settings)

;;; gtags-settings.el ends here
