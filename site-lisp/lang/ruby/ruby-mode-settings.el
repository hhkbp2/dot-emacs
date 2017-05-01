;;; ruby-mode-settings.el --- Settings for `ruby-mode'

;;; Commentary:

;;; Code:


(use-package enh-ruby-mode
  :defer t
  :ensure t
  :mode (("\\.rb$" . enh-ruby-mode)
         ("\\.rake$" . enh-ruby-mode)
         ("Rakefile$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.ru$" . enh-ruby-mode)
         ("Gemfile$" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode)
  :config
  (progn
    (setq enh-ruby-bounce-deep-indent t
          enh-ruby-hanging-brace-indent-level 2)

    (sp-with-modes '(rhtml-mode)
      (sp-local-pair "<" ">")
      (sp-local-pair "<%" "%>")))
  :bind
  (:map enh-ruby-mode-map
        ([(control d)] . c-hungry-delete-forward)
        ([(backspace)] . c-hungry-delete-backwards)
        ([(control c) (d)] . comment-dwim)
        ([(return)] . newline-and-indent))
  )

(provide 'ruby-mode-settings)

;;; ruby-mode-settings.el ends here
