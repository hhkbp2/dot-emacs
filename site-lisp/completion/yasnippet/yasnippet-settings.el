;;; yasnippet-settings.el --- Settings for `yasnippet'

;;; Commentary:

;;; Code:


;;;###autoload
(defun yasnippet-reload-after-save ()
  (let* ((bfn (expand-file-name (buffer-file-name)))
         (root (expand-file-name yas/root-directory)))
    (when (string-match (concat "^" root) bfn)
      (yas/load-snippet-buffer))))

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (progn
    (setq yas/root-directory (concat (current-directory) "snippets/"))

    (add-hook 'after-save-hook 'yasnippet-reload-after-save)

    (yas/global-mode 1))
  )

(provide 'yasnippet-settings)

;;; yasnippet-settings.el ends here
