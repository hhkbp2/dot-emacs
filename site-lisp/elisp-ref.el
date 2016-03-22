;;; elisp-ref.el --- Code from Emacs Lisp Reference Manual
;; -*- Emacs-Lisp -*-

;; Time-stamp: <2016-03-22 11:27>

;;; Commentary:

;;; Code:

(defvar fuzz-factor 1.0e-6)

(defun approx-equal (x y)
  (or (and (= x 0) (= y 0))
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         fuzz-factor)))

(provide 'elisp-ref)

;;; elisp-ref.el ends here
