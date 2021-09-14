;;; whitespace-settings.el --- Settings for `whitespace'

;;; Commentary:

;;; Code:


(use-package whitespace
  :defer t
  :config
  (progn
    (when (display-graphic-p)
      ;; refer to http://ergoemacs.org/emacs/whitespace-mode.html
      (setq whitespace-display-mappings
            ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
            '(
              (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
              (newline-mark 10 [8629 10]) ; 10 LINE FEED, 8629 DOWNWARDS ARROW WITH CORNER LEFTWARDS 「↵」
              (tab-mark 9 [8677 9] [92 9]) ; 9 TAB, 8677 RIGHTWARDS ARROW TO BAR 「⇥」
              )))
    )
  )

(provide 'whitespace-settings)

;;; whitespace-settings.el ends here
