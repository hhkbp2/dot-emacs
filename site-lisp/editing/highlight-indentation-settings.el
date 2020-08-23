;;; highlight-indentation-settings.el --- Settings for `highlight-indentation'

;;; Commentary:

;;; Code:


(defun highlight-indentation-face-settings()
  "Face settings for `highlight-indentation'."
  (custom-set-faces
   '(highlight-indentation-face
     ((((type x)) (:background "#333333"))
      (((class color) (min-colors 256)) (:background "#808080"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (t (:background "black"))))
   '(highlight-indentation-current-column-face
     ((((type x)) (:background "#333333"))
      (((class color) (min-colors 256)) (:background "#808080"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (t (:background "black"))))))

(use-package highlight-indentation
  :defer t
  :ensure t
  :init
  (progn
    (dolist (mode-hook '(c-mode-common-hook
                         python-mode-hook
                         erlang-mode-hook
                         html-mode-hook
                         conf-mode-hook))
      (add-hook mode-hook
                (lambda()
                  (highlight-indentation-current-column-mode)))))
  :config
  (progn
    (highlight-indentation-face-settings))
  )

(provide 'highlight-indentation-settings)

;;; highlight-indentation-settings.el ends here
