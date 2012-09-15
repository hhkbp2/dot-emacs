;; -*- Emacs-Lisp -*-
;; emacs lisp code in Learning GNU Emacs, 2rd
;; 《学习GNU Emacs, 第二版》
;; Debra Cameron, Bill Rosenblatt & Eric Raymond, Chinese translation, 2003


(defun count-words-buffer ()
  "Count the number of words in the current buffer;
print a message in the minibuffer with result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d word%s."
               count
               (if (> count 1)
                   "s"
                 "")))))


(defun count-lines-buffer ()
  "Count the number of lines in the current buffer;
print a message in the minibuffer with result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-line)
        (setq count (1+ count)))
      (message "buffer contains %d line%s."
               count
               (if (> count 1)
                   "s"
                 "")))))


(defun count-words-region (beg end)
  "Count the number of words in the current region;
print a message in the minibuffer with result."
  (interactive "r")
  (save-excursion
    (let ((count 0))
      (goto-char beg)
      (while (< (point) (point-max))
        (forward-word)
        (setq count (1+ count)))
      (message "region countains %d word%s."
               count
               (if (> count 1)
                   "s"
                 "")))))


(provide 'learning-gnu-emacs)
