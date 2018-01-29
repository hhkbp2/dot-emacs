;;; auto-complete-kapok.el --- Settings for `auto-complete' in kapok mode
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)
(require 'kapok-mode)


(defun ac-settings-4-kapok ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun auto-complete-kapok ()
  (ac-settings-4-kapok))

(am-add-hooks `(kapok-mode-hook)
              'auto-complete-kapok)

(provide 'auto-complete-kapok)

;;; auto-complete-kapok.el ends here
