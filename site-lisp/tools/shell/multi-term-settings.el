;;; multi-term-settings.el --- Settings for `multi-term'

;;; Commentary:
;;
;; `multi-term'
;; multiple terminal support
;; settings from ahei
;;

;;; Code:


(use-package multi-term
  :defer t
  :ensure t
  :config
  (progn
    (setq multi-term-switch-after-close nil)
    ;; jump to dedicated buffer when calling multi-term-dedicated-open
    (setq multi-term-dedicated-select-after-open-p t)
    (setq multi-term-program "/bin/zsh")
    (setq multi-term-buffer-name "multi-term")
    (setq term-unbind-key-list '("C-x" "<ESC>" "<up>" "<down>" "C-j"))
    (setq
     term-bind-key-alist
     `(;;("C-c"   . term-send-raw)
       ("C-p"   . term-send-raw)
       ("C-n"   . term-send-raw)
       ("C-s"   . isearch-forward)
       ("C-r"   . term-send-raw)
       ("C-m"   . term-send-raw)
       ("C-k"   . term-send-kill-line)
       ;;("C-y"   . term-send-raw)
       ("C-y"   . term-paste)
       (,(if window-system "C-/" "C-_") . term-send-undo)
       ("C-M-h" . term-send-backward-kill-semi-word)
       ("M-H"   . enter-text-mode)
       ("M-J"   . switch-term-and-text)
       ("M-f"   . term-send-raw-meta)
       ("M-b"   . term-send-raw-meta)
       ("M-d"   . term-send-raw-meta)
       ("M-K"   . term-send-kill-line)
       ("M-p"   . previous-line)
       ("M-n"   . next-line)
       ("M-u"   . term-send-raw-meta)
       ("M-w"   . term-send-copy-line)
       ("M-W"   . term-send-copy-line-left)
       ("M-y"   . term-send-raw-meta)
       ("M-."   . term-send-raw-meta)
       ("M-/"   . term-send-raw-meta)
       ("M-0"   . term-send-raw-meta)
       ("M-1"   . term-send-raw-meta)
       ("M-2"   . term-send-raw-meta)
       ("M-3"   . term-send-raw-meta)
       ("M-4"   . term-send-raw-meta)
       ("M-5"   . term-send-raw-meta)
       ("M-6"   . term-send-raw-meta)
       ("M-7"   . term-send-raw-meta)
       ("M-8"   . term-send-raw-meta)
       ("M-9"   . term-send-raw-meta)
       ("<left>"  . backward-char)
       ("<right>" . forward-char)))
    )
  :bind
  (([f9] . multi-term)
   :map text-mode-map
   ([(meta \[)] . enter-text-mode)
   ([(meta \])] . enter-term-mode))
  )

;;;###autoload
(defun term-send-kill-whole-line ()
  "Kill whole line in term mode."
  (interactive)
  (term-send-raw-string "\C-a")
  ;; 没有`sleep-for'有时候就不行, 不知道为什么
  (sleep-for 0 1)
  (kill-line)
  (term-send-raw-string "\C-k"))

;;;###autoload
(defun term-send-kill-line ()
  "Kill line in term mode."
  (interactive)
  (call-interactively 'kill-line)
  (term-send-raw-string "\C-k"))

;;;###autoload
(defun term-send-yank ()
  "Yank in term mode."
  (interactive)
  (yank)
  (term-send-raw-string (current-kill 0)))

;;;###autoload
(defun term-send-copy-line ()
  "Copy left line in term mode."
  (interactive)
  (term-send-home)
  (sleep-for 0 1)
  (term-send-copy-line-left))

;;;###autoload
(defun term-send-copy-line-left ()
  "Copy current left line in term mode."
  (interactive)
  (term-send-kill-line)
  (term-send-raw-string "\C-_"))

;;;###autoload
(defun term-send-backward-kill-semi-word ()
  "Backward kill semiword in term mode."
  (interactive)
  (term-send-raw-string "\e\C-h"))

;;;###autoload
(defun term-send-undo ()
  "Undo in term mode."
  (interactive)
  (term-send-raw-string "\C-_"))

;;;###autoload
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

;;;###autoload
(defun switch-term-and-text ()
  "if current in `term-mode', switch to `text-mode', else switch to `term-mode'."
  (interactive)
  (if (equal major-mode 'term-mode)
      (text-mode)
    (enter-term-mode)))

;;;###autoload
(defun enter-term-mode ()
  "Enter in `term-mode'."
  (interactive)
  (term-mode)
  (term-char-mode))

;;;###autoload
(defun enter-text-mode ()
  "Enter in `text-mode'."
  (interactive)
  (text-mode))

(provide 'multi-term-settings)

;;; multi-term-settings.el ends here
