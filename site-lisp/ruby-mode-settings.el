;;; ruby-mode-settings.el --- Settings for `ruby-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:



(defun ruby-mode-settings ()
  "Settings for `ruby-mode'."

  )

(eval-after-load "enh-ruby-mode"
  `(ruby-mode-settings))


(defun enh-ruby-mode-settings ()
  "Settings for `enh-ruby-mode'."
  (setq enh-ruby-bounce-deep-indent t
        enh-ruby-hanging-brace-indent-level 2)

  (sp-with-modes '(rhtml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>"))

  (define-key enh-ruby-mode-map "\C-d" 'c-hungry-delete-forward)
  (define-key enh-ruby-mode-map "\177" 'c-hungry-delete-backwards)
  (define-key enh-ruby-mode-map [?\C-c ?\d] 'comment-dwim)
  (define-key enh-ruby-mode-map (kbd "RET") 'newline-and-indent)
  )

(eval-after-load "enh-ruby-mode"
  `(enh-ruby-mode-settings))

(dw-add-file-mode-pattern-list '(("\\.rb$" . enh-ruby-mode)
                                 ("\\.rake$" . enh-ruby-mode)
                                 ("Rakefile$" . enh-ruby-mode)
                                 ("\\.gemspec$" . enh-ruby-mode)
                                 ("\\.ru$" . enh-ruby-mode)
                                 ("Gemfile$" . enh-ruby-mode))
                               t)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'ruby-mode-settings)

;;; ruby-mode-settings.el ends here
