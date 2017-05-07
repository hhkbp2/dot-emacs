;;; hs-minor-mode-settings.el --- Settings for `hs-minor-mode'

;;; Commentary:
;;
;; `hs-minor-mode':
;; fold and unfold code

;;; Code:


;;;###autoload
(defvar hs-headline-max-len 30
  "*Maximum length of `hs-headline' to display.")

;;;###autoload
(defun hs-display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs-headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs-headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))

;;;###autoload
(defun hs-abstract-overlay (ov)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (str (format "<%d lines>" (count-lines start end))) text)
    (setq text
          (propertize str
                      'face 'hs-block-flag-face 'help-echo
                      (buffer-substring (1+ start) end)))
    (overlay-put ov 'display text)
    (overlay-put ov 'pointer 'hand)
    (overlay-put ov 'keymap hs-overlay-map)))

(use-package hideshow
  :defer t
  :ensure t
  :config
  (progn
    (setq hs-isearch-open t)

    (setq-default mode-line-format
                  (append '((:eval (hs-display-headline))) mode-line-format))

    (setq hs-set-up-overlay 'hs-abstract-overlay)
    )
  :bind
  (:map hs-minor-mode-map
        ([(control c) (\,) (H)] . hs-hide-all)
        ([(control c) (\,) (S)] . hs-show-all)
        ([(control c) (\,) (h)] . hs-hide-block)
        ([(control c) (\,) (s)] . hs-show-block)
        ([(control c) (\,) (l)] . hs-hide-level)
        ([(control c) (\,) (t)] . hs-toggle-hiding)
        ;; use mouse to hide/show
        ([(C-down-mouse-1)] . hs-toggle-hiding))
  )

(provide 'hs-minor-mode-settings)

;;; hs-minor-mode-settings.el ends here
