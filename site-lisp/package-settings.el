;;; package-settings.el --- Settings for the package manager `package'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(require 'my-subdirs)

;; After loading the init file and abbrev file(if there is any),
;; emacs 24 will run some code like these to load installed packages.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))
;; And then variable `package-archives' will be set to the default value
;; '(("gnu" . "http://elpa.gnu.org/packages/"))


(defvar dw-package-addition-sources
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/"))
  "Addtional package sources.")

(defvar dw-package-list
  '(
    undo-tree
    redo+

    color-theme
    cal-china-x

    dired+
    dired-details
    dired-details+
    fiplr
    popwin
    neotree
    windresize

    ido-complete-space-or-hyphen
    ido-ubiquitous
    helm
    helm-ag
    helm-swoop
    helm-descbinds
    helm-describe-modes
    smex

    google-this
    youdao-dictionary
    guide-key

    auto-complete
    ac-ispell
    yasnippet
    gtags
    flycheck
    highlight-indentation
    highlight-symbol
    highlight-tail
    multi-term

    exec-path-from-shell
    dash-at-point
    osx-dictionary

    rainbow-delimiters
    smartparens
    slime
    geiser
    ac-geiser

    clojure-mode
    cider

    google-c-style

    erlang
    elixir-mode

    python-mode
    pymacs
    jedi
    python-pep8
    python-pylint

    enh-ruby-mode

    rust-mode
    racer
    ac-racer
    flycheck-rust

    go-mode
    go-autocomplete
    go-eldoc
    golint
    go-errcheck

    thrift
    protobuf-mode
    scss-mode
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

(defun package-settings ()
  (dolist (source dw-package-addition-sources)
    (add-to-list 'package-archives source))
  (my-add-subdirs-to-load-path package-user-dir))


(eval-after-load "package"
  `(package-settings))


(provide 'package-settings)

;;; package-settings.el ends here
