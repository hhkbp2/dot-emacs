;;; elisp-intro.el --- Code from the book "elisp intro"
;; -*- Emacs-Lisp -*-

;; Emacs Lisp code in Programming in Emacs Lisp: An Introduction, 2rd
;; Robert J. Chassell, Chinese translation, 2001
;; 《GNU Emacs Lisp编程入门》第二版中文版

;; Time-stamp: <2016-03-22 14:34>

;;; Commentary:

;;; Code:

;;; chapter 13

(defun count-words-region-iteratively (beginning end)
  "Print number of words in the region (iterative implementation)."
  (interactive "r")
  (message "Counting words in region ... ")
  ;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      ;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      ;; 3. Send a message to the user.
      (cond ((zerop count)
             (message "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word."))
            (t
             (message "The region has %d words." count))))))


(defun recursive-count-words (region-end)
  "Number of words between point and REGION-END."
  ;; 1. do-again-test
  (if (and (< (point) region-end)
           (re-search-forward "\\w+\\W*" region-end t))
      ;; 2. then-part: the recursive call
      (1+ (recursive-count-words region-end))
    0))


(defun count-words-region-recursively (beginning end)
  "Print number of words in the region (recursive implementation)."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (goto-char beginning)
    (let (count (recursive-count-words end))
      (cond ((zerop count)
             (message "The region does NOT have any words."))
            ((= 1 count)
             (message "The region has 1 word."))
            (t
             (message "The region has %d words." count))))))


;;; chapter 14

(defun count-words-in-defun ()
  "Return the number of words and symbols in a defun."
  (beginning-of-defun)
  (let ((count 0)
        (end (save-excursion (end-of-defun) (point))))
    (while (and (< (point) end)
                (re-search-forward "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*" end t))
      (setq count (1+ count)))
    count))


(defun count-words-defun ()
  "Number of words and symbols in a function definition."
  (interactive)
  (message "Counting words and symbols in function definition ...")
  (let ((count (count-words-in-defun)))
    (cond ((zerop count)
           (message "The definition does NOT have any words or symbols."))
          ((= 1 count)
           (message "The definition has 1 word or symbol."))
          (t
           (message "The definition has %d words or symbols." count)))))


(defun lengths-list-file (filename)
  "Return list of definitions' lengths within FILE.
The returned list is a list of numbers.
Each number is the number of words or
symbols in one function definition."
  (message "Working on '%s' ... " filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          (length-list))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^(defun" nil t)
        (setq length-list
              (cons (count-words-in-defun) length-list)))
      (kill-buffer buffer)
      length-list)))


(defun lengths-list-many-files (list-of-files)
  "Return list of lengths of defuns in LIST-OF-FILES."
  (let (lengths-list)
    ;; true-of-false-test
    (while list-of-files
      (setq lengths-list
            (append lengths-list
                    ;; Generate a lengths' list
                    (lengths-list-file (expand-file-name (car list-of-files)))))
      ;; Make files' list shorter.
      (setq list-of-files (cdr list-of-files)))
    ;; Return final value of lengths' list.
    lengths-list))


(defun recursive-lengths-list-many-files (list-of-files)
  "Return list of lengths of each defun in LIST-OF-FILES."
  (if list-of-files
      (append
       (lengths-list-file (expand-file-name (car list-of-files)))
       (recursive-lengths-list-many-files (cdr list-of-files)))))


(defvar top-of-ranges
  '(10 20 30 40 50
       60 70 80 90 100
       110 120 130 140 150
       160 170 180 190 200
       210 220 230 240 250
       260 270 280 290 300)
  "List specifying ranges for `defuns-per-range'.")


(defun defuns-per-range (sorted-lengths top-of-ranges)
  "SORTED-LENGTHS defuns in each TOP-OF-RANGES range."
  (let ((top-of-range (car top-of-ranges))
        (number-within-range 0)
        defuns-per-range-list)
    ;; Outer loop
    (while top-of-ranges
      ;; Inner loop
      (while (and
              ;; Need number for numeric test.
              (car sorted-lengths)
              (< (car sorted-lengths) top-of-range))
        ;; Count number of definitions within current range.
        (setq number-within-range (1+ number-within-range))
        (setq sorted-lengths (cdr sorted-lengths)))

      ;; Exit Inner loop but remain within outer loop
      (setq defuns-per-range-list
            (cons number-within-range defuns-per-range-list))
      (setq number-within-range 0) ;; Reset count to zero

      ;; Move to next range.
      (setq top-of-ranges (cdr top-of-ranges))
      ;; Specify next top of range value.
      (setq top-of-range (car top-of-ranges)))

    ;; Exit outer loop and count the number of defuns larger than
    ;; the largets top-of-range value.
    (setq defuns-per-range-list
          (cons
           (length sorted-lengths)
           defuns-per-range-list))

    ;; Return a list of the number of definitions within each range,
    ;; smallest to largest.
    (nreverse defuns-per-range-list)))


;;; chapter 15

(defvar graph-symbol "*"
  "String used as symbol in graph, usually an asterisk.")


(defvar graph-blank " "
  "String used as blank in graph, usually a blank space.
graph-blank must be the same number of columns wide as graph-symbol.")


(defun column-of-graph (max-graph-height actual-height)
  "Return list of MAX-GRAPH-HEIGHT strings;
ACTUAL-HEIGHT are graph-symbols.
The graph-symbols are contiguous entries at the end of the list.
The list will be inserted as one column of a graph.
The strings are either graph-blank or graph-symbol."
  (let ((insert-list nil)
        (number-of-top-blanks (- max-graph-height actual-height)))
    ;; Fill in graph-symbols.
    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1-  actual-height)))

    ;; Fill in graph-blanks.
    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks (1- number-of-top-blanks)))
    ;; Return whole list.
    insert-list))


(defun recursive-graph-body-print-internal (numbers-list height symbol-width)
  "Print a bar graph. Used within recursive-graph-body-print function."
  (if numbers-list
      (progn
        (setq from-position (point))
        (insert-rectangle
         (column-of-graph height (car numbers-list)))
        (goto-char from-position)
        (forward-char symbol-width)
        (sit-for 0) ;; Draw graph column by column.
        (recursive-graph-body-print-internal
         (cdr numbers-list)
         height
         symbol-width))))


(defun recursive-graph-body-print (numbers-list)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values."
  (let ((height (apply 'max numbers-list))
        (symbol-width (length graph-blank))
        from-position)
    (recursive-graph-body-print-internal
     numbers-list
     height
     symbol-width)))


;;; chapter 16

;;; Keybinding for `occur'
;; I use occur a lot, so let's bind it to a key:
;;(global-set-key "\C-co" 'occur)
;;(global-set-key [(control c) (o)] 'occur)


;;; Line to top of window;
;;; replace three keystroke sequence C-u 0 C-l
(defun line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive)
  (recenter 0))


;;; chapter 17

;; a function's definition with bug
(defun triangle-bugged (number)
  "Return sum of numbers 1 through NUMBER inclusive."
  (let ((total 0))
    (while (> number 0)
      (setq total (+ total number))
      (setq number (1= number))) ;; Error here.
    total))


;; a function's recursive definition with bug
(defun triangle-recursively-bugged (number)
  "Return sum of numbers 1 through NUMBER inclusive.
Uses recursion."
  (if (= number 1)
      1
    (+ number
       (triangle-recursively-bugged
        (1= number))))) ;; Error here.


;;; appendix c

;;; graph Y axis

(defvar Y-axis-label-spacing 5
  "Number of lines from one Y axis label to next.")

(defvar Y-axis-tic " - "
  "String that follows number in a Y axis label.")


(defun Y-axis-element (number full-Y-label-width)
  "Construct a NUMBERed label element.
A numbered element looks like this ` 5 -',
and is padded as needed so all line up with
the element for the largest number."
  (let* ((leading-spaces
          (- full-Y-label-width
             (length
              (concat (int-to-string number)
                      Y-axis-tic)))))
    (concat
     (make-string leading-spaces ? )
     (int-to-string number)
     Y-axis-tic)))


(defun Y-axis-column (height width-of-label &optional vertical-step)
  "Construct list of labels for Y axis.
HEIGHT is maximum height of graph.
WIDTH-OF-LABEL is maximum width of label.
VERTICAL-STEP, an option, is a positive integer
that specifies how much a Y axis label increments
for each line. For example, a step of 5 means
that each line is five units of the graph."
  (let (Y-axis
        (number-per-line (or vertical-step 1)))
    (while (> height 1)
      (if (zerop (% height Y-axis-label-spacing))
          ;; Insert label.
          (setq Y-axis
                (cons
                 (Y-axis-element (* height number-per-line) width-of-label)
                 Y-axis))
        ;; Else, insert blanks.
        (setq Y-axis
              (cons
               (make-string width-of-label ? )
               Y-axis)))
      (setq height (1- height)))
    ;; Insert base line
    (setq Y-axis (cons (Y-axis-element
                        number-per-line
                        width-of-label)
                       Y-axis))
    (nreverse Y-axis)))


(defun print-Y-axis (height full-Y-label-width &optional vertical-step)
  "Insert Y axis using HEIGHT and FULL-Y-LABEL-WIDTH.
Height must be the maximum height of the graph.
Full width is width of the highest label element.
Optionally, print according to VERTICAL-STEP."
  ;; Value of height and full-Y-label-width
  ;; are passed by `print-graph'.
  (let ((start (point)))
    (insert-rectangle
     (Y-axis-column height full-Y-label-width vertical-step))
    ;; Place point ready for inserting graph.
    (goto-char start)
    ;; Move point forward by value of full-Y-label-width
    (forward-char full-Y-label-width)))


;;; graph Y axis test

;; (print-Y-axis 12 5)


;;; graph X axis

(defvar X-axis-label-spacing
  (if (boundp 'graph-blank)
      (* 5 (length graph-blank))
    5)
  "Number of units from one X axis label to next.")


(defvar X-axis-tic-symbol "|"
  "String to insert to point to a column in X axis.")


(defun print-X-axis-tic-line
  (number-of-X-tics X-axis-leading-spaces X-axis-tic-element)
  "Print tics for X axis."
  (insert X-axis-leading-spaces)
  (insert X-axis-tic-symbol) ;; Under first column.
  ;; Insert second tic in the right spot.
  (insert (concat
           (make-string
            (- (* symbol-width X-axis-label-spacing)
               ;; Insert white space up to second tic symbol.
               (* 2 (length X-axis-tic-symbol)))
            ? )
           X-axis-tic-symbol))
  ;; Insert remaining tics.
  (while (> number-of-X-tics 1)
    (insert X-axis-tic-element)
    (setq number-of-X-tics (1- number-of-X-tics))))


(defun X-axis-element (number)
  "Construct a numbered X axis element."
  (let ((leading-spaces
         (- (* symbol-width X-axis-label-spacing)
            (length (int-to-string number)))))
    (concat (make-string leading-spaces ? )
            (int-to-string number))))


(defun print-X-axis-numbered-line
  (number-of-X-tics X-axis-leading-spaces
                    &optional horizontal-step)
  "Print line of X-axis numbers"
  (let ((number X-axis-label-spacing)
        (horizontal-step (or horizontal-step 1)))
    (insert X-axis-leading-spaces)
    ;; Delete extra leading spaces.
    (delete-char
     (- (1-
         (length (number-to-string horizontal-step)))))
    (insert (concat
             (make-string
              ;; Insert white space.
              (-  (* symbol-width
                     X-axis-label-spacing)
                  (1-
                   (length
                    (number-to-string horizontal-step)))
                  2)
              ? )
             (number-to-string
              (* number horizontal-step))))
    ;; Insert remaining numbers.
    (setq number (+ number X-axis-label-spacing))
    (while (> number-of-X-tics 1)
      (insert (X-axis-element
               (* number horizontal-step)))
      (setq number (+ number X-axis-label-spacing))
      (setq number-of-X-tics (1- number-of-X-tics)))))


(defun print-X-axis (numbers-list horizontal-step)
  "Print X axis labels to length of NUMBERS-LIST.
     Optionally, HORIZONTAL-STEP, a positive integer,
     specifies how much an X  axis label increments for
     each column."
  ;; Value of symbol-width and full-Y-label-width
  ;; are passed by `print-graph'.
  (let* ((leading-spaces
          (make-string full-Y-label-width ? ))
         ;; symbol-width is provided by graph-body-print
         (tic-width (* symbol-width X-axis-label-spacing))
         (X-length (length numbers-list))
         (X-tic
          (concat
           (make-string
            ;; Make a string of blanks.
            (-  (* symbol-width X-axis-label-spacing)
                (length X-axis-tic-symbol))
            ? )
           ;; Concatenate blanks with tic symbol.
           X-axis-tic-symbol))
         (tic-number
          (if (zerop (% X-length tic-width))
              (/ X-length tic-width)
            (1+ (/ X-length tic-width)))))

    (print-X-axis-tic-line
     tic-number leading-spaces X-tic)
    (insert "\n")
    (print-X-axis-numbered-line
     tic-number leading-spaces horizontal-step)))


;;; graph X axis test

;; (progn
;;   (let ((full-Y-label-width 5)
;;         (symbol-width 1))
;;     (print-X-axis
;;      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))


;;; graph body

(defun graph-body-print (numbers-list height symbol-width)
  "Print a bar graph of the NUMBERS-LIST.
The numbers-list consists of the Y-axis values.
HEIGHT is maximum height of graph.
SYMBOL-WIDTH is number of each column."
  (let (from-position)
    (while numbers-list
      (setq from-position (point))
      (insert-rectangle
       (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      ;; Draw graph column by column.
      (sit-for 0)
      (setq numbers-list (cdr numbers-list)))
    ;; Place point for X axis labels.
    (forward-line height)
    (insert "\n")))


;;; graph

(defun print-graph
  (numbers-list &optional vertical-step horizontal-step)
  "Print labeled bar graph of the NUMBERS-LIST.
     The numbers-list consists of the Y-axis values.

     Optionally, VERTICAL-STEP, a positive integer,
     specifies how much a Y axis label increments for
     each line.  For example, a step of 5 means that
     each row is five units.

     Optionally, HORIZONTAL-STEP, a positive integer,
     specifies how much an X  axis label increments for
     each column."
  (let* ((symbol-width (length graph-blank))
         ;; `height' is both the largest number
         ;; and the number with the most digits.
         (height (apply 'max numbers-list))
         (height-of-top-line
          (if (zerop (% height Y-axis-label-spacing))
              height
            ;; else
            (* (1+ (/ height Y-axis-label-spacing))
               Y-axis-label-spacing)))
         (vertical-step (or vertical-step 1))
         (full-Y-label-width
          (length
           (concat
            (number-to-string
             (* height-of-top-line vertical-step))
            Y-axis-tic))))
    (print-Y-axis
     height-of-top-line full-Y-label-width vertical-step)
    (graph-body-print
     numbers-list height-of-top-line symbol-width)
    (print-X-axis numbers-list horizontal-step)))


;;; graph test

(defun one-fiftieth (full-range)
  "Return list, each number one-fiftieth of previous."
  (mapcar '(lambda (arg) (/ arg 50)) full-range))


;; (print-graph (one-fiftieth
;;               (defuns-per-range
;;                 (sort
;;                  (lengths-list-many-files
;;                   (directory-files "/usr/share/emacs/site-lisp/erc" t "\\.el$"))
;;                  '<)
;;                 top-of-ranges))
;;              10 10)


(provide 'elisp-intro)

;;; elisp-intro.el ends here
