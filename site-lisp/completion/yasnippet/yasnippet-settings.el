;;; yasnippet-settings.el --- Settings for `yasnippet'

;;; Commentary:

;;; Code:


;;;###autoload
(defun yasnippet-reload-after-save ()
  (let* ((bfn (expand-file-name (buffer-file-name)))
         (root (expand-file-name yas/root-directory)))
    (when (string-match (concat "^" root) bfn)
      (yas/load-snippet-buffer))))


(defvar dw-yas-snippets-dir
  (concat (current-directory) "snippets/")

  "Directory of snippets for `yasnippet'.
Defined before loading `yasnippet' so as to preserve current directory
since the loading is defered.")


(use-package yasnippet
  :defer t
  :ensure t
  :config
  (progn
    (add-to-list 'yas-snippet-dirs dw-yas-snippets-dir)

    (add-hook 'after-save-hook 'yasnippet-reload-after-save)

    (yas/global-mode 1))
  )

(provide 'yasnippet-settings)

;;; yasnippet-settings.el ends here
