;;; markdown-mode-settings.el --- Settings for `markdown-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'whitespace)
(require 'dw-functionals)


(autoload 'markdown-mode "markdown-mode.el"
"Majar mode for editing Markdown files" t)

(defun markdown-mode-settings ()
  "Settings for `markdown-mode'."

  (setq whitespace-style
        '(face trailing indentation tabs tab-mark))
  (whitespace-mode 1)
  )

(add-hook 'markdown-mode-hook
          'markdown-mode-settings)

(dw-add-file-mode-pattern-list '(("\\.markdown$" . markdown-mode)
                                 ("\\.md$" . markdown-mode)))


(provide 'markdown-mode-settings)

;;; markdown-mode-settings.el ends here
