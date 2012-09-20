;; -*- Emacs-Lisp -*-
;; dw-functionals.el --- Dylan.Wen's elisp functionality

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-09-20 17:01>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'cl)


(defgroup dw-functionals nil
  "Dylan.Wen's functional codes."
  :group 'dw-functionals
  :prefix "dw-")


;;; movement utils

;; move to positions of line

(defvar move-position-of-line-last-op nil
  "Indicates the last `move-position-of-line' operation performed.
Possible values: `beginning', `middle', `end'.")


(defcustom move-position-of-line-positions '(middle beginning end)
  "Cycling order for `move-position-of-line'.
A list of elements with possible values `beginning', `middle', `end',
which define the cycling order for the command `move-position-of-line'.

Beginning is the start point of line, and end is the end point
of it as displayed. However middle means the first non-whitespace charater
from beginning of line, which is the first non-whitespace character.
Refer to 35.2.1 Table of Syntax Classes of The Emacs Lisp Reference Manual
for more information about whitespace charater and non-whitespace charater.
The default cycliing order is middle -> beginning -> end.")


(defun move-to-indentation (&optional arg)
  "Move point to the first non-whitespace charater of line as displayed.

With a integer argument ARG not nil or 1, move forward or backward,
which depends on whether ARG is positive or negative, | ARG - 1 | lines first.
If point reaches the beginning of end of buffer, it stops there."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line arg)
  (back-to-indentation))


(defun move-position-of-line (&optional arg)
  "Move point to beginning, the first non-whitespace charater or end of line.

A port and enhancement from the `Emacs' key binding mode in eclipse,
where you press `C-a' key sequence continuously and the insert point
jumps between the beginning and the first character of current line
on each key press.

With a integer argument ARG not nil or 1, move forward or backward,
which depends on whether ARG is positive or negative, | ARG - 1 | lines first.
If point reaches the beginning or end of buffer, it stops there."
  (interactive "P")
  (setq move-position-of-line-last-op
        (if (eq this-command last-command)
            (car (or (cdr (member move-position-of-line-last-op
                                  move-position-of-line-positions))
                     move-position-of-line-positions))
          (car move-position-of-line-positions)))
  (cond
   ((eq move-position-of-line-last-op 'middle)
    (move-to-indentation arg))
   ((eq move-position-of-line-last-op 'beginning)
    (beginning-of-line arg))
   ((eq move-position-of-line-last-op 'end)
    (end-of-line arg))))


;; override the default key binding of C-a
(global-set-key [(control a)] 'move-position-of-line)


;;; editing utils

;;;###autoload
(defmacro def-redo-command (fun-name redo undo)
  "Make redo command."
  `(defun ,fun-name ()
     (interactive)
     (if (equal last-command ,redo)
         (setq last-command 'undo)
       (setq last-command nil))
     (call-interactively ,undo)
     (setq this-command ,redo)))


(defun copy-word (&optional arg)
  "Copy word at point."
  (interactive "P")
  (let ((beg (progn (if (looking-back "\\sw" 1)
                        (backward-word 1))
                    (point)))
        (end (progn (forward-word arg)
                    (point))))
    (copy-region-as-kill beg end)))


(defun copy-line (&optional arg)
  "copy current line."
  (interactive "P")
  (let ((beg (line-beginning-position arg))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))


(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end)))


(defun dw-camelcase-to-underscore (start end)
  "Convert any string matching something like aBc to a_bc."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char 1)
      (let ((case-fold-search nil))
        (while (search-forward-regexp "\\([a-z0-9]\\)\\([A-Z]\\)" nil t)
          (replace-match (concat (match-string 1)
                                 "_"
                                 (downcase (match-string 2)))
                         t nil))))))


(defun dw-underscore-to-camelcase (start end)
  "Convert any string matching something like a_bc to aBc."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char 1)
      (let (case-fold-search)
        (while (search-forward-regexp "\\([a-zA-Z0-9]\\)\\(_\\)\\([a-zA-Z0-9]\\)" nil t)
          (replace-match (concat (match-string 1)
                                 (capitalize (match-string 3)))
                         t nil)
          (backward-char))))))


;; delete trailing carriage return

(defconst dw-carriage-return 13
  "carriage-return(^M) ascii code.")


(defun dw-remove-carriage-return ()
  "Remove all carriage-return(^M) at end of line in buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward
                (concat (char-to-string dw-carriage-return) "$")
                (point-max) t)
          (replace-match "" nil nil)
          (setq remove-count (1+ remove-count)))
        (message (format "%d ^M removed from buffer" remove-count))))))


;;; language utils

(defun disjoin (fn &rest fns)
  "Return a predicate that return true when any of the predicates return true."
  ;; ugly code to enable lexical binding as common lisp
  (lexical-let ((fn fn)
                (fns fns))
    (if (null fns)
        fn
      (lexical-let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args)))))))

(defun conjoin (fn &rest fns)
  "Return a predicate that return true when all of the predicates return true."
  (lexical-let ((fn fn)
                (fns fns))
    (if (null fns)
        fn
      (lexical-let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args)))))))


;;; load utils

(defun* dw-load-file-if-exist (&key file (dir nil))
  "Load the specified file if it exists.
Optional `dir' specifies the directory where the file locates."
  (let ((file-to-load
         (if dir
             (concat dir file)
           file)))
    (and (funcall (conjoin #'file-exists-p #'file-readable-p) file-to-load)
         (load-file file-to-load))))

(defun dw-load-related-file-if-exist (file)
  "Load file that is related to current directory if it exists."
  (dw-load-file-if-exist
   :file file
   :dir (file-name-directory (or load-file-name buffer-file-name))))


;;; debug utils

(defun dw-debug-print-list (lst)
  "Print list `lst' in current buffer or echo area."
  (if lst
      (dolist (elem lst)
        (print (format "%S" elem) (or (current-buffer) t)))))


;; file and string utils

(defun read-file (file)
  "Read the content of a file into a String."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (buffer-string))))


(defun read-file-nth-line (file &optional line-number)
  "Return a single nth line of file `file'.
With argument `line-number' not nil, return the corresponding nth line.
E.g. To read the first line of file, set `line-number' 1. To read
the second line, set `line-number' 2."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (beginning-of-line line-number)
      (let ((line-begin-point (point)))
        (end-of-line)
        (buffer-substring line-begin-point (point))))))


(defun empty-string-p (str)
  "Return t if the string `str' is empty."
  (if (= (length str) 0)
      t
    nil))


;; platform/machine utils

(defun system-distribution ()
  "Return current machine's distribution information in pair (name, version).
The distribution information is from the file `/etc/issue'.
E.g. On Ubuntu 9.04, it returns (\"Ubuntu\", \"9.04\").
On Ubuntu 12.04.1 LTS, it returns (\"Ubuntu\", \"12.04.1\")."
  (let* ((dist-info
          (split-string (read-file-nth-line "/etc/issue") " " t)))
    (cond
     ((null dist-info) (values "" ""))
     (t (values (first dist-info) (second dist-info))))))


(defun dw-on-office-machine ()
  "Return t if it runs on my office machines."
  (multiple-value-bind (dist-name dist-verison) (system-distribution)
    (cond
     ((and (equal dist-name "Ubuntu")) t)
     (t nil))))


(provide 'dw-functionals)
