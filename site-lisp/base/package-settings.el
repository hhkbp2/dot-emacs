;;; package-settings.el --- Settings for the package manager `package'

;;; Commentary:

;;; Code:


;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)


;; After loading the init file and abbrev file(if there is any),
;; emacs 24 will run some code like these to load installed packages.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))
;; And then variable `package-archives' will be set to the default value
;; '(("gnu" . "http://elpa.gnu.org/packages/"))

(require 'my-subdirs)
(require 'use-package)


(defvar dw-package-addition-sources
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.org/packages/"))
  "Addtional package sources.")

(defvar dw-package-list
  '(
    popwin
    neotree
    windresize

    youdao-dictionary
    guide-key

    auto-complete
    yasnippet
    flycheck
    highlight-indentation
    highlight-symbol
    highlight-tail

    exec-path-from-shell
    osx-dictionary

    ac-geiser

    google-c-style

    erlang
    elixir-mode

    python-mode

    go-mode
    go-autocomplete

    yaml-mode
    toml-mode
    json-mode
    ))

(defun dw-package-recompile-all ()
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun dw-package-check-all-installed ()
  (interactive)
  (dolist (p dw-package-list)
    (when (not (package-installed-p p))
      (package-install p))))

(use-package package
  :defer t
  :config
  (progn
    (dolist (source dw-package-addition-sources)
      (add-to-list 'package-archives source))
    (my-add-subdirs-to-load-path package-user-dir))
  )

(provide 'package-settings)

;;; package-settings.el ends here
