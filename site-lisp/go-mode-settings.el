;;; go-mode-settings.el --- Settings for `go-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'golint)
(require 'dw-functionals)
(require 'go-mode)

(defun dw-go-tidb-gopath ()
  "Detect GOPATH for the tidb project."
  (seq-some #'dw-go--tidb-gopath ["_vendor" "vendor"]))

(defun dw-go--tidb-gopath (filename)
  "Detect GOPATH using the hint FILENAME."
  (let* ((d (locate-dominating-file buffer-file-name "tidb"))
         (tidb-root (concat d (file-name-as-directory "tidb")))
         (vendor (concat tidb-root (file-name-as-directory filename))))
    (if (and d
             (file-exists-p vendor))
        (cons vendor (go-plain-gopath)))))

(defadvice gofmt (before gofmt-prepare-gopath activate)
  "Detect GOPATH and set it before running `gofmt'."
  (go-set-project))

(defadvice gofmt (after gofmt-reset-gopath activate)
  "Reset GOPATH to the value it had before running `gofmt'."
  (go-reset-gopath))

(defun go-mode-settings ()
  "Settings for `go-mode'."

  ;; run goimports before saving golang buffer
  ;; non `go-mode' buffer would be intact
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; add the gopath guessing function for tidb
  (add-to-list 'go-guess-gopath-functions #'dw-go-tidb-gopath)

  ;; key bindings
  (dw-hungry-delete-on-mode-map go-mode-map)
  (dw-commet-dwin-on-mode-map go-mode-map)

  ;; Enable `subword-mode' since go is Camel style.
  (add-hook 'go-mode-hook
            '(lambda ()
               ;; turn on `go-eldoc'
               (go-eldoc-setup)

               ;; turn on `subword-mode' since golang uses camel case.
               (subword-mode)
               ;; keybindings
               (local-set-key (kbd "M-.") 'godef-jump)))
  )

(eval-after-load "go-mode"
  `(go-mode-settings))


(provide 'go-mode-settings)

;;; go-mode-settings.el ends here
