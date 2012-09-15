;; -*- Emacs-Lisp -*-

;;;; settings for highlight symbol
;; Time-stamp: <2011-01-29 18:28>


(require 'util)
(require 'highlight-symbol)

(defun highlight-symbol-settings ()
  "Settings for `highlight-symbol'."

  (setq highlight-symbol-idle-delay 0.5)

  (defun highlight-symbol-mode-on ()
    "Turn on function `highlight-symbol-mode'."
    (highlight-symbol-mode 1))

  (defun highlight-symbol-mode-off ()
    "Turn off function `highlight-symbol-mode'."
    (highlight-symbol-mode -1))

;;;###autoload
  (define-globalized-minor-mode
    global-highlight-symbol-mode
    highlight-symbol-mode
    highlight-symbol-mode-on)

;;;###autoload
  (defun highlight-symbol-jump (dir)
    "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
    (let ((symbol (highlight-symbol-get-symbol)))
      (if symbol
          (let* ((case-fold-search nil)
                 (bounds (bounds-of-thing-at-point 'symbol))
                 (offset (- (point)
                            (if (< 0 dir)
                                (cdr bounds)
                              (car bounds)))))
            (unless (eq last-command 'highlight-symbol-jump)
              (push-mark))
            (let ((target
                   (save-excursion
                     ;; move a little, so we don't find the same instance again
                     (goto-char (- (point) offset))
                     (re-search-forward symbol nil t dir))))
              (if target
                  (goto-char (+ target offset))
                (message (format "Reach %s" (if (> dir 0) "bottom" "top"))))
              (setq this-command 'highlight-symbol-jump)))
        (error "No symbol at point"))))

  (global-set-key (kbd "C-c M-h") 'highlight-symbol-at-point)
  (global-set-key (kbd "C-c M-r") 'highlight-symbol-remove-all)
  (global-set-key (kbd "C-c M-n") 'highlight-symbol-next)
  (global-set-key (kbd "C-c M-p") 'highlight-symbol-prev)

  (defun highlight-symbol-settings-4-emaci ()
    "`highlight-symbol' settings for `emaci'."
    (emaci-add-key-definition "n" 'highlight-symbol-next)
    (emaci-add-key-definition "p" 'highlight-symbol-prev))

  (eal-define-keys
   'emaci-mode-map
   `(("p" emaci-p)))

  (eval-after-load "emaci"
    `(highlight-symbol-settings-4-emaci))

  (eal-define-keys
   `(emacs-lisp-mode-map lisp-interaction-mode-map java-mode-map
                         c-mode-base-map text-mode-map ruby-mode-map html-mode-map)
   `(("C-c M-H" highlight-symbol-at-point)
     ("C-c M-R" highlight-symbol-remove-all)
     ("C-c M-N" highlight-symbol-next)
     ("C-c M-P" highlight-symbol-prev)
     ("C-c r"   highlight-symbol-query-replace)
     ("C-c M-n" highlight-symbol-next-in-defun)
     ("C-c M-p" highlight-symbol-prev-in-defun))))

(eval-after-load "highlight-symbol"
  '(highlight-symbol-settings))


(provide 'highlight-symbol-settings)