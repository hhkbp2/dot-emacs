;;; python-mode-settings.el --- Settings for `python-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'whitespace)

(require 'pymacs-settings)
(require 'python-ropemacs-settings)
(require 'jedi-settings)
(require 'dw-functionals)
(require 'python-mode)

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

(defun python-ropemacs-load()
  "Load ropemacs library."

  ;; (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-global-prefix "C-c p")

  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os" "sys"))
  (ropemacs-mode t))

(use-package pymacs
  :defer t
  :config
  (progn
    (autoload 'pymacs-apply "pymacs")
    (autoload 'pymacs-call "pymacs")
    (autoload 'pymacs-eval "pymacs" nil t)
    (autoload 'pymacs-exec "pymacs" nil t)
    (autoload 'pymacs-load "pymacs" nil t)

    ;; additional module search path
    (setq pymacs-load-path `(,dw-python-dev-dir ,dw-python-path)))
  )

(use-package python-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; This line is needed to fix the bug:
    ;; "Symbol's function definition is void: tramp-tramp-file-p"
    ;; when `python-pylint' or `python-pep8' starts up.
    (require 'tramp)

    (require 'python-pylint-autoloads)
    (require 'python-pep8-autoloads)

    (when (eq system-type 'darwin)
      ;; prepare PYTHONPATH for mac emacs as app started on dock
      (dw-prepare-pypath))

    (require 'pymacs)

    ;; set tab width
    (setq tab-width 4)

    ;; show whitespace/tab
    (setq whitespace-style
          '(face indentation::tab indentation::space tabs tab-mark trailing))
    (whitespace-mode 1)

    ;; load ropemacs
    (python-ropemacs-load)

    ;; key bindings
    (dw-hungry-delete-on-mode-map python-mode-map)
    (dw-commet-dwin-on-mode-map python-mode-map))
  :bind
  (:map python-mode-map
        (([(return)] . py-newline-and-indent)))
  )

(provide 'python-mode-settings)

;;; python-mode-settings.el ends here
