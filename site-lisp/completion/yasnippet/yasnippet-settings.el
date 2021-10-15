;;; yasnippet-settings.el --- Settings for `yasnippet'

;;; Commentary:

;;; Code:


;;;###autoload
(defun yasnippet-reload-after-save ()
  (let ((bfn (expand-file-name (buffer-file-name))))
    (some (lambda (dir)
            (when (string-match dir bfn)
              (yas-load-snippet-buffer 'yasnippet)))
          yas-snippet-dirs)))

(defvar dw-yas-snippets-dir
  (concat (current-directory) "snippets/")

  "Directory of snippets for `yasnippet'.
Defined before loading `yasnippet' so as to preserve current directory
since the loading is defered.")


(use-package yasnippet
  :defer t
  :ensure t
  :config
  (setq yas-snippet-dirs (list dw-yas-snippets-dir))
  (add-hook 'snippet-mode-hook (lambda ()
                                 (add-hook 'after-save-hook 'yasnippet-reload-after-save nil 'local)))

  (yas/global-mode 1)
  )

(provide 'yasnippet-settings)

;;; yasnippet-settings.el ends here
