;;; occur-settings.el --- Settings for `occur-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


;;; occur on current symbol

(defconst occur-default-output-buffer-name "*Occur*"
  "The default output buffer name of command `occur'.")

;; define customized occur commands
(defun occur-symbol (&optional nlines)
  "Run command `occur' on current word.
To simplify the interface to `occur' when programming."
  (interactive "P")
  (let ((symbol nil)
        (output-window nil)
        (match-line-prefix nil)
        (match-position nil))
    ;; get the regexp to search:
    ;; 1. if the point is within a symbol, then regexp is current symbol
    ;; 2. or, prompt on the minibuffer for user to input a regexp
    ;; the later one's behaviour is the same with canonical command `occur'
    (setq symbol
          (concat "\\_<"
                  (or (current-word 'WITHIN-SYMBOL)
                      (read-regexp "List lines matching regexp"
                                   (car regexp-history)))
                  "\\_>")
          match-line-prefix
          (concat "^[[:blank:]]*"
                  (int-to-string (line-number-at-pos))
                  ":"))
    ;; call `occur-1' to perform the actual search and display actions
    (occur-1 symbol nlines (list (current-buffer)))
    ;; jump to the first occurrence in the output window
    (and (setq output-window
               (get-buffer-window occur-default-output-buffer-name))
         (select-window output-window)
         (re-search-forward match-line-prefix (point-max) t 1)
         (setq match-position (next-single-property-change (point) 'occur-match))
         (goto-char match-position))))

(global-set-key [(control c) (o)] 'occur-symbol)


;;; movements in occur mode

(defun occur-find-match-quiet (n search)
  "Find the Nth match that fits function SEARCH, and do it quietly.
Comparing to `occur-find-match', if it fails, return the list
'(nil number-of-rewind-matches)rather than trigger a error.
return the list '(t position-of-buffer) the buffer position if succeeds.
In all cases it never move the point."
  (let ((position (point)))
    ;; every match string is highlighted to have two property change,
    ;; one is the first character fo the match string
    ;; the other is the point right after the final character of it
    ;; so here we need to double the value N
    (setq n (* 2 n))
    ;; get Nth match on one direction if possible
    (while (and (> n 0)
                (setq position (funcall search position 'occur-match)))
      (setq n (1- n)))
    (if (> n 0)
        ;; there is no as many as N match on one direction
        (and (setq n (/ (1+ n) 2))
             (list nil n))
      ;; we get the Nth match position of buffer
      (list t position))))


(defun occur-prev-rewind (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer.
If there is no more previous match, rewind to the last match in current buffer."
  (interactive "p")
  (or n (setq n 1))
  (let ((result-list))
    (cond
     ((= n 0)
      t) ;; donot move the point
     ((> n 0)
      (if (and (setq
                result-list
                (occur-find-match-quiet n #'previous-single-property-change))
               (car result-list))
          ;; the required match doesnot go beyond the first match
          ;; jump to the precise point directly
          (let ((position (car (cdr result-list))))
            (goto-char position))
        ;; the required match goes beyond the first match, rewind to bottom
        (let ((number-of-rewind-matches (car (cdr result-list)))
              (position (point))
              (position-after-last-match nil))
          ;; go to the point right after final match
          (while (setq
                  position
                  (funcall #'next-single-property-change position 'occur-match))
            (setq position-after-last-match position))
          ;; backward to the start point of match, which is my favour :)
          (and (setq
                position
                (funcall #'previous-single-property-change
                         position-after-last-match 'occur-match))
               (goto-char position))
          ;; backward `number-of-rewind-matches' - 1  matches, recursively
          (occur-prev-rewind (1- number-of-rewind-matches)))))
     ((< n 0)
      (occur-next-rewind (- n))))))


(defun occur-next-rewind (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer.
If there is no more next match, rewind to the first match in current buffer."
  (interactive "p")
  (or n (setq n 1))
  (let ((result-list))
    (cond
     ((= n 0)
      t) ;; do not move the point
     ((> n 0)
      (if (and (setq
                result-list
                (occur-find-match-quiet n #'next-single-property-change))
               (car result-list))
          ;; the required match doesnot go below the final match
          ;; jump to the precise point directly
          (let ((position (car (cdr result-list))))
            (goto-char position))
        ;; the required match goes below the final match, rewind to top
        (let ((number-of-rewind-matches (car (cdr result-list)))
              (position (point))
              (position-at-first-match nil))
          ;; go to the point right at first match
          (while (setq
                  position
                  (funcall #'previous-single-property-change
                           position 'occur-match))
            (setq position-at-first-match position))
          (and position-at-first-match
               (goto-char position-at-first-match))
          ;; forward `number-of-rewind-matches' - 1 matches, recursively
          (occur-next-rewind (1- number-of-rewind-matches)))))
     ((< n 0)
      (occur-prev-rewind (- n))))))


(defun occur-settings ()
  "Settings for `occur-mode'."

  ;; reference:
  ;; `list-matching-lines-default-context-lines'
  ;; display N lines before and after the matching regexp
  ;; N default value is 0
  ;; N < 0 : include N lines only before the match
  ;; N > 0 : include N lines both before and after
  ;; --
  ;; I get used to display a few line before and after the match
  ;; to see the context
  (setq list-matching-lines-default-context-lines 2)

  ;; simplify key bindings of jumping to the previous and next match
  (define-key occur-mode-map [(p)] 'occur-prev-rewind)
  (define-key occur-mode-map [(n)] 'occur-next-rewind))

(add-hook 'occur-mode-hook
          'occur-settings)


(provide 'occur-settings)

;;; occur-settings.el ends here
