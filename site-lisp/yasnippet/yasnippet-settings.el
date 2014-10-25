;; -*- Emacs-Lisp -*-

;;;; settings for yasnippet
;; Time-stamp: <2014-10-25 14:26>


(require 'yasnippet)


(defun yasnippet-settings ()
  "Settings for `yasnippet'."
  (setq yas/root-directory (concat (current-directory) "snippets/"))

;;;###autoload
  (defun yasnippet-reload-after-save ()
    (let* ((bfn (expand-file-name (buffer-file-name)))
           (root (expand-file-name yas/root-directory)))
      (when (string-match (concat "^" root) bfn)
        (yas/load-snippet-buffer))))
  (add-hook 'after-save-hook 'yasnippet-reload-after-save)

  (yas/global-mode 1)
  )


(eval-after-load "yasnippet"
  `(yasnippet-settings))

;;(yas/load-directory yas/root-directory)


(provide 'yasnippet-settings)
