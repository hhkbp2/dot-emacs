;; -*- Emacs-Lisp -*-
;; Emacs lisp code ported from the book On Lisp
;; Paul Graham, Prentice Hall, 1993


(proclaim '(inline last1 single append1 conc1 mklist))
(proclaim '(optimize speed))

;;; figure 4.1

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; figure 4.2

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (e lst)
      (let ((val (funcall fn e)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;; figure 4.3
;; TODO

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))


(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max score))))
      (values wins max))))

(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

;; usage:
;; (combine 2 3)
;; (combine '(a b) '(c d))
(defun combine (&rest args)
  (apply (combiner (car args))
         args))


(defun compose (&rest fns)
  "Return a function that apply all functions call in `fns'."
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))


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


(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x) #'(lambda (&rest args) x))


(provide 'on-lisp)
