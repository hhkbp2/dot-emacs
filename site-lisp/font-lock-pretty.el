;; -*- Emacs-Lisp -*-
;; A few features to make `font-lock-mode' pretty

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-10 00:26>

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


(require 'wlc)


(defvar lw-cc-paren-regexp
  "[](){}[]"
  "Regex used to search c/c++ parentheses.")


(defcustom lw-cc-paren-nonhighlighted-face-list
  '(font-lock-comment-face font-lock-string-face font-lock-doc-face
                           font-lock-doc-string-face)
  "*A list of face where makefile parenthese wouldn't be highlighted.")


(defvar lw-makefile-left-paren-regexp
  "\\$("
  "Regex used to search makefile left parentheses.")


(defvar lw-makefile-right-paren-regexp
  ")"
  "Regex used to search makefile right parentheses.")


(defvar lw-makefile-paren-regexp
  "\\(\\$(\\|)\\)"
  "Regex used to search makefile parentheses.")


(defvar lw-makefile-built-in-function-regexp
  "\\$[({]\\(call\\|eval\\|filter\\(?:-out\\)?\\|findstring\\|\
subst\\|patsubst\\|word\\(?:s\\|list\\)?\\|firstword\\|sort\\|shell\\|\
wildcard\\|dir\\|notdir\\|suffix\\|basename\\|addsuffix\\|addprefix\\|\
join\\|if\\|error\\|foreach\\|strip\\|origin\\|warning\\)[:=#) \t\n]"
  "Regex used to search makefile built-in function.")


(defcustom lw-makefile-keyword-nonhighlighted-face-list
  '(font-lock-comment-face)
  "*A list of face where makefile parenthese wouldn't be highlighted.")


;; `font-lock-keywords' matcher for c/c++ parentheses
(defun lw-match-cc-paren (bound)
  "Search for a c/c++ parentheses in pair up to BOUND."
  (wlc/match-paren lw-cc-paren-regexp
                   lw-cc-paren-nonhighlighted-face-list
                   bound))


;; generic `font-lock-keywords' matcher for parentheses (based on regex)
(defun lw-match-paren (paren-regexp
                       left-paren-regexp right-paren-regexp
                       nonhighlighted-face-list bound side)
  "Search for a parentheses in pair up to BOUND.
PAREN-REGEXP should be a regexp that matches both left parentheses and
right parenthesesis, the `(match-beginning 1)' of it should be either
LEFT-PAREN-REGEXP or RIGHT-PAREN-REGEXP.
LEFT-PAREN-REGEXP should be a regexp that matches only left parentheses.
RIGHT-PAREN-REGEXP should be a regexp that mathces only right parentheses.
BOUND is the limit of searching.
SIDE has two valid values: `left' `right', means to search for the left
parentheses or right parentheses separately."
  (unless (or (equal side 'left)
              (equal side 'right))
    (error "error in function lw-match-paren: invalid argument."))
  (catch 'found
    (let (paren-target-regexp
          paren-pair-regexp
          pre-search-pair-action
          re-search-pair
          bound-search-pair)
      (if (equal side 'left)
          (setq paren-target-regexp left-paren-regexp
                paren-pair-regexp right-paren-regexp
                pre-search-pair-action nil
                re-search-pair 're-search-forward
                bound-search-pair bound)
        (setq paren-target-regexp right-paren-regexp
              paren-pair-regexp left-paren-regexp
              pre-search-pair-action '(lambda ()
                                        (goto-char (match-beginning 0)))
              re-search-pair 're-search-backward
              bound-search-pair (point-min)))
      (while (re-search-forward paren-target-regexp bound t)
        ;; a target parentheses is found
        (if (wlc/position-has-no-face (match-beginning 0)
                                      nonhighlighted-face-list)
            ;; the found target parentheses isn't in nonhighlighted face
            (let ((target-paren-counter 1)
                  (buffered-match-data (match-data)))
              ;; search for the parentheses in pair matched `paren-pair-regexp'
              (save-excursion
                ;; do pre search pair action if there is any
                (and pre-search-pair-action
                     (apply pre-search-pair-action nil))
                (while (and (> target-paren-counter 0)
                            (apply re-search-pair paren-regexp
                                   bound-search-pair t nil))
                  ;; a parentheses is found
                  (if (wlc/position-has-no-face (match-beginning 0)
                                                nonhighlighted-face-list)
                      ;; the found parentheses isn't in nonhighlighted face
                      (if (string-match paren-pair-regexp (match-string 1))
                          ;; it is a parentheses which we are searching
                          (setq target-paren-counter (1- target-paren-counter))
                        ;; it is a same parentheses as `paren-target-regexp'
                        (setq target-paren-counter (1+ target-paren-counter))))))
              (unless (> target-paren-counter 0)
                ;; the target parentheses has its 'pair', highlight it
                (set-match-data buffered-match-data)
                (throw 'found t))))))))


;; `font-lock-keywords' matcher for makefile left parentheses
(defun lw-match-makefile-left-paren (bound)
  "Search for a left makefile parentheses in pair up to BOUND."
  (lw-match-paren lw-makefile-paren-regexp
                  lw-makefile-left-paren-regexp
                  lw-makefile-right-paren-regexp
                  lw-makefile-keyword-nonhighlighted-face-list
                  bound
                  'left))


;; `font-lock-keywords' matcher for makefile right parentheses
(defun lw-match-makefile-right-paren (bound)
  "Search for a right makefile parentheses in pair up to BOUND."
  (lw-match-paren lw-makefile-paren-regexp
                  lw-makefile-left-paren-regexp
                  lw-makefile-right-paren-regexp
                  lw-makefile-keyword-nonhighlighted-face-list
                  bound
                  'right))


;; `font-lock-keywords' matcher for makefile built-in function
(defun lw-match-makefile-built-in-function (bound)
  "Search for makefile built in function up to BOUND."
  (catch 'found
    (while (re-search-forward lw-makefile-built-in-function-regexp bound t)
      ;; (if (wlc/position-has-no-face (1- (point))
      ;;                              lw-makefile-keyword-nonhighlighted-face-list)
      (throw 'found t)
      ;;)
      )))


(provide 'font-lock-pretty)
