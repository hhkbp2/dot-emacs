;;; python-mode-settings.el --- Settings for `python-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(defconst dw-python-dev-dir (expand-file-name "~/pro/python")
  "Personal development code directory of python.")

(defconst dw-python-path (expand-file-name
                          "~/local/lib/python2.7/site-packages/")
  "Personal PYTHONPATH for addtional libraries.")

(defun dw-prepare-pypath ()
  (let ((pypath (getenv "PYTHONPATH")))
    (if (or (null pypath) (string= "" pypath))
        (setenv "PYTHONPATH" dw-python-path)
      (if (not (search dw-python-path pypath))
          (setenv "PYTHONPATH" (concat dw-python-path ":" pypath))))))

(use-package python-mode
  :defer t
  :ensure t
  :config
  (progn
    (require 'whitespace)

    (when (eq system-type 'darwin)
      ;; prepare PYTHONPATH for mac emacs as app started on dock
      (dw-prepare-pypath))

    ;; set tab width
    (setq tab-width 4)

    ;; show whitespace/tab
    (setq whitespace-style
          '(face indentation::tab indentation::space tabs tab-mark trailing))
    (whitespace-mode 1)

    ;; key bindings
    (dw-hungry-delete-on-mode-map python-mode-map)
    (dw-commet-dwin-on-mode-map python-mode-map))
  :bind
  (:map python-mode-map
        (([(return)] . py-newline-and-indent)))
  )

(provide 'python-mode-settings)

;;; python-mode-settings.el ends here
