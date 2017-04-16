;;; highlight-indentation-settings.el --- Settings for `highlight-indentation'
;; -*- Emacs-Lisp -*-

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

(defun highlight-indentation-settings()
  "Settings for `highlight-indentation'."

  (highlight-indentation-face-settings)
  )

(eval-after-load "highlight-indentation"
  `(highlight-indentation-settings))

(dolist (mode-hook '(c-mode-common-hook
                     java-mode-hook
                     enh-ruby-mode-hook
                     python-mode-hook
                     erlang-mode-hook
                     js-mode-hook
                     html-mode-hook
                     conf-mode-hook))
  (add-hook mode-hook
            (lambda () (highlight-indentation-current-column-mode))))


(provide 'highlight-indentation-settings)

;;; highlight-indentation-settings.el ends here
