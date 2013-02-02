;; -*- Emacs-Lisp -*-
;; Emacs lisp code ported from the book On Lisp
;; Paul Graham, Prentice Hall, 1993


(proclaim '(inline last1 single append1 conc1 mklist))
(proclaim '(optimize speed))

;;;; chapter 4

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
                (lexical-let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;; figure 4.3

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                      ((consp (car tree))
                       (rec (cdr tree)
                            (cons (rec (car tree) nil) acc)))
                      (t (rec (cdr tree)
                              (if (funcall test (car tree))
                                  acc
                                (cons (car tree) acc)))))))
    (rec tree nil)))

;; figure 4.4

(defun find2 (fn lst)
  (if (null lst)
      nil
    (lexical-let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (lexical-let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;; figure 4.5

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max  score))))
      (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
            (setq wins obj)))
      wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max    score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))

;; figure 4.6

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

;; figure 4.7

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~A~%" (funcall fn in))))))

;; figure 4.8

(defun mkstr (&rest args)
  (with-output-to-string (s)
                         (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))

;;;; chapter 5

;;; figure 5.1

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;;; figure 5.2

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

;; figure 5.3

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; figure 5.4

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
        (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
    (lexical-let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
    (lexical-let ((chain (apply #'fun fns)))
      #'(lambda (x)
          (or (funcall fn x) (funcall chain x))))))

;; figure 5.5

(defun lrec (rec &optional base)
  (labels ((self (lst)
                 (if (null lst)
                     (if (functionp base)
                         (funcall base)
                       base)
                   (funcall rec (car lst)
                            #'(lambda ()
                                (self (cdr lst)))))))
    #'self))

;; figure 5.6

;; copy-list
;;(lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
;;(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
;;(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
;;(lrec #'(lambda (x f) (or (fn x) (funcall f))))



;; TODO

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
