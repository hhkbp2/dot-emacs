;;; learning-gnu-emacs-calc-mode.el --- Calc Mode from book Learning GNU Emacs
;; -*- Emacs-Lisp -*-

;; Time-stamp: <2016-03-22 12:05>

;;; Commentary:

;; A calculator major mode, which is an example of making a new major mode in
;; Learning GNU Emacs, 2rd
;; 《学习GNU Emacs, 第二版》
;; Debra Cameron, Bill Rosenblatt & Eric Raymond, Chinese translation, 2003
;;
;;  Supports the operators +, -, *, /, and % (remainder).
;;  Commands:
;;  c   clear the stack
;;  =   print the value at the top of the stack
;;  p   print the entire stack contents

;;; Code:


(defvar calc-mode-map nil
  "Local keymap for calculator mode buffers.")

; set up the calculator mode keymap with
; C-j (linefeed) as "eval" key
(if calc-mode-map
    nil
  (setq calc-mode-map (make-sparse-keymap))
  (define-key calc-mode-map "?\C-j" 'calc-eval))

(defconst calc-number-regexp
  "-?\\([0-9]+\\.?\\|\\.\\)[0-9]*\\(e[0-9]+\\)?"
  "Regular expression for recognizing numbers.")

(defconst calc-operator-regexp
  "[-+*/%]"
  "Regular expression for recognizing operators.")

(defconst calc-command-regexp
  "[c=ps]"
  "Regular expression for recognizing commands.")

(defconst calc-whitespace
  "[ \t]"
  "Regular expression for recognizing whitespace.")

;; beep and print and error message
(defun calc-error-message (str)
  (beep)
  (message str)
  nil)

;; stack functions
(defun calc-push (num)
  (if (numberp num)
      (setq calc-stack (cons num calc-stack))))

(defun calc-top ()
  (if (not calc-stack)
      (error-message "stack empty.")
    (car calc-stack)))

(defun calc-pop ()
  (let ((val (calc-top)))
    (if val
        (setq calc-stack (cdr calc-stack)))
    val))

;; functions for user commands
(defun calc-print-stack ()
  "Print entire contents of stack, from top to bottom."
  (if calc-stack
      (progn
        (insert "\n")
        (let ((stk calc-stack))
          (while calc-stack
            (insert (number-to-string (calc-pop)) " "))
            (setq calc-stack stk)))
    (error-message "stack empty.")))

(defun calc-clear-stack ()
  "Clear the stack."
  (setq calc-stack nil)
  (message "stack cleared."))

(defun calc-command (tok)
  "Given a command token, perform the appropriate action."
  (cond ((equal tok "c")
         (calc-clear-stack))
        ((equal tok "=")
         (insert "\n" (number-to-string (calc-top))))
        ((equal tok "p")
         (calc-print-stack))
        (t
         (message (concat "invalid command: " tok)))))

(defun calc-operate (tok)
  "Given an arithmetic operator (as string), pop two numbers
off the stack, perform operation tok (given as string), push the result onto the stack."
  (let ((op1 (calc-pop))
        (op2 (calc-pop)))
    (calc-push (funcall (read tok) op2 op1))))

(defun calc-push-number (tok)
  "Given a number (as string), push it (as number ) on to the stack."
  (calc-push (string-to-number tok)))

(defun calc-invalid-tok (tok)
  (error-message-string (concat "Invalid token: " tok))
  (sit-for 0))  ; make sure message is displayed

(defun calc-next-token ()
  "Pick up the next token, based on regexp search.
As side effects, advance point one past the token,
and set name of function to usb to process the token."
  (let (tok)
    (cond ((looking-at calc-number-regexp)
           (goto-char (match-end 0))
           (setq calc-proc-fun 'calc-push-number))
          ((looking-at calc-operator-regexp)
          (forward-char 1)
          (setq calc-proc-fun 'calc-operate))
          ((looking-at calc-command-regexp)
           (forward-char 1)
           (setq calc-proc-fun 'calc-command))
          (t
           (setq calc-proc-fun 'calc-invalid-tok)))
    ;; pick up token and advance past it (and past whitespace)
    (setq tok (buffer-substring (match-beginning 0) (point)))
    (if (looking-at calc-whitespace)
        (goto-char (match-end 0)))
    tok))

(defun calc-eval ()
  "Main evaluation function for calculator mode.
Process All tokens on an input line."
  (interactive)
  (beginning-of-line)
  (while (not (eolp))
    (let ((tok (calc-next-token)))
      (funcall calc-proc-fun tok)))
  (insert "\n"))

(defun calc-mode ()
  "Calculator mode, using H-P style postfix notation.
Understands the arithmetic operators +, -, *, / and %,
plus the following commands:
    c   clear the stack
    =   print top of stack
    p   print entire stack contents (top to bottom)
Linefedd (C-j) is bound to an evaluation function that
will evaluate everything on the current line. No
whitespace is necessary, except to separate numbers."
  (interactive)
  (pop-to-buffer "*Calc*" nil)
  (kill-all-local-variables)
  (make-local-variable 'calc-stack)
  (setq calc-stack nil)
  (make-local-variable 'calc-proc-fun)
  (setq major-mode 'calc-mode)
  (setq mode-name "Calculator")
  (use-local-map calc-mode-map))

(provide 'calc-mode)

;;; learning-gnu-emacs-calc-mode.el ends here
