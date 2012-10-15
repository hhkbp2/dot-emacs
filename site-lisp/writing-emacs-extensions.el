;; -*- Emacs-Lisp -*-
;; emacs lisp code in Writing GNU Emacs Extensions, 1st
;; Bob Glickstein, O'REILLY, 1997


;; windows switching
(global-set-key "\C-x\C-n" 'other-window)

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-x\C-p" 'other-window-backward)


;; scroll
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)

(global-set-key "\C-x\C-q" 'quoted-insert)


;; Cursor motion

(defvar move-cursor-to-window-last-op nil
  "Indicates the last `move-cursor-to-window' operation performed.
Possible values: `top', `bottom'.")


(defcustom move-cursor-to-window-positions '(top bottom)
  "Cycling order for `move-cursor-to-window' operations.
A list of elments with possible values `top', `bottom',
which define the cycling order for the command `move-cursor-to-window'.")


(defun move-cursor-to-top ()
  "Put cursor at beginning of first visible line of current window."
  (interactive)
  (move-to-window-line 0))

(global-set-key "\M-," 'move-cursor-to-top)
(global-set-key "\C-x," 'tags-loop-continue)


(defun move-cursor-to-bottom ()
  "Put cursor at beginning of last visible line of current window."
  (interactive)
  (move-to-window-line -1))

(global-set-key "\M-." 'move-cursor-to-bottom)


(defun move-cursor-to-window (&optional arg)
  "Put cursor at beginning of first or last visible line of current window."
  (interactive "P")
  (setq move-cursor-to-window-last-op
        (if (eq this-command last-command)
            (car (or (cdr (member move-cursor-to-window-last-op
                                  move-cursor-to-window-positions))
                     move-cursor-to-window-positions))
          (car move-cursor-to-window-positions)))
  (cond
   ((eq move-cursor-to-window-last-op 'top)
    (move-cursor-to-top))
   ((eq move-cursor-to-window-last-op 'bottom)
    (move-cursor-to-bottom))))

;; It is somewhat tricky using the key ; in key combination.
(global-set-key (kbd "C-;") 'move-cursor-to-window)
;; The statement above also works as
;;(global-set-key [(control \;)] 'move-cursor-to-window)


(defcustom move-line-to-window-margin 4
  "*Number of lines of margin at top and bottom of a window.
used in `move-line-to-window'.")


(defvar move-line-to-window-last-op nil
  "Indicates the last `move-line-to-window' operation performed.
Possible values: `top', `middle', `bottom'.")


(defun move-line-to-top ()
  "Move the line of point to the top of current window."
  (interactive)
  (recenter 0))


(defun move-line-to-bottom()
  "Move the line of point to the bottom of current window."
  (interactive)
  (recenter -1))


(defun move-line-to-window(&optional arg)
  "Move the line of point to the top, middle or bottom of current window.
This command works in the samiliar way of `recenter-top-bottom', however
it uses `move-line-to-window-margin' instead of `scroll-margin'."
  (interactive "P")
  (cond
   (arg (recenter arg))    ;; Always respect ARG.
   ((or (not (eq this-command last-command))
        (eq move-line-to-window-last-op 'bottom))
    (setq move-line-to-window-last-op 'middle)
    (recenter))
   (t
    (let ((this-scroll-margin
           (min (max 0 move-line-to-window-margin)
                (truncate (/ (window-body-height) 4.0)))))
      (cond
       ((eq move-line-to-window-last-op 'middle)
        (setq move-line-to-window-last-op 'top)
        (recenter this-scroll-margin))
       ((eq move-line-to-window-last-op 'top)
        (setq move-line-to-window-last-op 'bottom)
        (recenter (- -1 this-scroll-margin))))))))

;; using "C-l" to toggle `recenter-top-bottom' already defined in `window.el',
;; no need to redefine similar `move-line' key binding here
;; (global-set-key "\C-l" 'move-line-to-window)


;;; Symbolic link handling

(defun visit-symbolic-read-only ()
  "Mark currrent buffer read only if it's symbolic."
  (interactive)
  (if (file-symlink-p buffer-file-name)
      (progn
        (setq buffer-read-only t)
        (message "File is a symbolic link"))))

;; ;; mark buffer to be read-only if read in a symbolic link file
;; (add-hook 'find-file-hooks
;;           'visit-symbolic-read-only)


(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name))
            (symlink-maybe nil))
        (if target
            (progn
              (setq symlink-maybe (file-relative-name buffer-file-name))
              (find-alternate-file target)
              (message "visit file [%s] instead of symlink [%s]"
                       target symlink-maybe))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

;; visit the target file instead of symbolic link
(add-hook 'find-file-hooks
          'visit-target-instead)


(defun clobber-symlink()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (if (yes-or-no-p (format "Replace %s with %s?"
                                     buffer-file-name
                                     target))
                (progn
                  (delete-file buffer-file-name)
                  (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))


;; Advised buffer switching
(defadvice switch-to-buffer (before existing-buffer
                                    activate compile)
  "When interactive, switch only to existing buffers,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))



;; Advised buffer switching in other window
(defadvice switch-to-buffer-other-window (before existing-buffer-other-window
                                                 activate compile)
  "When interactive, switch only to existing buffers in other window,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))


;; Advised buffer switching in other frame
(defadvice switch-to-buffer-other-frame (before existing-buffer-other-frame
                                                activate compile)
  "When interactive, switch only to existing buffers in other frame,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null (current-prefix-arg))))))


;; unscroll
(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-left 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to `unscroll'.")
(defvar unscroll-hscroll nil
  "Hscroll for next call to `unscroll'.")

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                               activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
                               activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
                                activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))


(global-set-key [(meta r)] 'move-cursor-to-window)
(global-set-key [(control x) (l)] 'move-line-to-window)
(global-set-key [(control x) (control l)] 'move-line-to-window)
(global-set-key [(meta o)] 'unscroll)


(provide 'writing-emacs-extensions)
