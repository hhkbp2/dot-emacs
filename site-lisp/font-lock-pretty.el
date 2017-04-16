;;; font-lock-pretty.el --- A few features to make `font-lock-mode' pretty
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defvar dw-makefile-built-in-function-regexp
  "\\$[({]\\(call\\|eval\\|filter\\(?:-out\\)?\\|findstring\\|\
subst\\|patsubst\\|word\\(?:s\\|list\\)?\\|firstword\\|sort\\|shell\\|\
wildcard\\|dir\\|notdir\\|suffix\\|basename\\|addsuffix\\|addprefix\\|\
join\\|if\\|error\\|foreach\\|strip\\|origin\\|warning\\)[:=#) \t\n]"
  "Regex used to search makefile built-in function.")


(defcustom dw-makefile-keyword-nonhighlighted-face-list
  '(font-lock-comment-face)
  "*A list of face where makefile parenthese wouldn't be highlighted.")


(defun dw-position-has-face (pos list-face)
  "Return t if there is any face on position POS of buffer in list LIST-FACE."
  (unless (and (integer-or-marker-p pos)
               (listp list-face))
    (error "error in function dw-position-has-face: invalid argument."))
  (let ((face-on-position (get-text-property pos 'face)))
    (catch 'has-face
      (dolist (face list-face)
        (when (or (and (listp face-on-position)
                       (memq face face-on-position))
                  (eq face face-on-position))
          (throw 'has-face t))))))


(defun dw-position-has-no-face (pos list-face)
  "Return t if there is no face on position POS of buffer in list LIST-FACE."
  (not (dw-position-has-face pos list-face)))


;; `font-lock-keywords' matcher for makefile built-in function
(defun dw-match-makefile-built-in-function (bound)
  "Search for makefile built in function up to BOUND."
  (catch 'found
    (while (re-search-forward dw-makefile-built-in-function-regexp bound t)
      ;; (if (dw-position-has-no-face (1- (point))
      ;;                              dw-makefile-keyword-nonhighlighted-face-list)
      (throw 'found t)
      ;;)
      )))


(provide 'font-lock-pretty)

;;; font-lock-pretty.el ends here
