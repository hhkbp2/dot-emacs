;;; color-theme+.el --- An enhancement of `color-theme'

;;; Commentary:

;;; Code:


(require 'color-theme)


(defgroup color-theme+ nil
  "An enhancement of `color-theme'."
  :group 'faces)


;; redifine the function `color-theme-spec-compat' of `color-theme'
(if (or (featurep 'xemacs)
        (< emacs-major-version 21))
    (defalias 'color-theme-spec-compat 'identity)
(defun color-theme-spec-compat (spec)
  "An redifination of `color-theme-spec-compat' of `color-theme'.
To apply different face settings according to different term types."
  :group 'color-theme+
  (let (compat-spec props)
    (dolist (dis-atts spec)
      (setq props (cadr dis-atts))
      (when (plist-member props :bold)
        (setq props (color-theme-plist-delete props :bold))
        (unless (plist-member props :weight)
          (setq props (plist-put props :weight 'bold))))
      (when (plist-member props :italic)
        (setq props (color-theme-plist-delete props :italic))
        (unless (plist-member props :slant)
          (setq props (plist-put props :slant 'italic))))
      (setcdr dis-atts `(,props))
      (add-to-list 'compat-spec dis-atts 'APPEND))
    compat-spec)))


;; (color-theme-spec-compat '((t (:foreground "blue" :bold t))))
;; (color-theme-spec-compat
;; '((t (:bold t :foreground "blue" :weight extra-bold))))
;; (color-theme-spec-compat '((t (:italic t :foreground "blue"))))
;; (color-theme-spec-compat
;; '((t (:slant oblique :italic t :foreground "blue"))))
;; (color-theme-spec-compat
;; '((((class color) (min-colors 88) (background dark))
;;    (:bold t :foreground "#96ff00"))
;;   (((class color) (min-colors 88) (background light))
;;    (:bold t :foreground "#96ff00"))
;;   (((class color) (min-colors 16) (background dark))
;;    (:foreground "limegreen" :italic))
;;   (((class color) (min-colors 16) (background light))
;;    (:foreground "limegreen" :italic))
;;   (((class color) (min-colors 8) (background dark))
;;    (:foreground "green"))
;;   (((class color) (min-colors 8) (background light))
;;    (:foreground "green"))
;;   (((type tty) (class mono)) (:foreground "green"))
;;   (t (:foreground "#96ff00"))))


(provide 'color-theme+)

;;; color-theme+.el ends here
