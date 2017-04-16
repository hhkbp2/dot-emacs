;;; awk-mode-settings.el --- Settings for `awk-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'cc-mode)


;; 定制`awk-mode'缩进风格
(defconst dw-awk-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     (defun-open after)
     (defun-close . c-snug-1line-defun-close)
     (substatement-open after)
     (block-close . c-snug-do-while)
     (arglist-cont-nonempty))
    (c-hanging-semi&comma-criteria)
    (c-cleanup-list)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open . 0)
     (statement-cont . +)))
  "Dylan.Wen's awk indentation style based on built-in style `awk'.")


(defun awk-mode-settings ()
  "Settings for `awk-mode'."
  (c-add-style "dw-awk-style" dw-awk-style)
  (c-set-style "dw-awk-style"))


(add-hook 'awk-mode-hook
          'awk-mode-settings)


(provide 'awk-mode-settings)

;;; awk-mode-settings.el ends here
