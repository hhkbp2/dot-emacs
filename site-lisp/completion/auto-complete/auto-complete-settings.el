;;; auto-complete-settings.el --- Settings for `auto-complete'
;; -*- Emacs-Lisp -*-

;; A loan from ahei
;; Time-stamp: <2017-11-03 15:09>

;;; Commentary:

;;; Code:


(require 'auto-complete-config)
(require 'auto-complete+)
(require 'auto-complete-cc)
(require 'auto-complete-python)
(require 'auto-complete-lisp)
(require 'auto-complete-joxa)
(require 'auto-complete-kapok)
(require 'go-autocomplete)
(require 'util)


;; After do this, isearch any string, M-: (match-data) always
;; return the list whose elements is integer
(global-auto-complete-mode 1)


;; 不让回车的时候执行`ac-complete', 因为当你输入完一个
;; 单词的时候, 很有可能补全菜单还在, 这时候你要回车的话,
;; 必须要干掉补全菜单, 很麻烦, 用M-j来执行`ac-complete'
(eal-define-keys
 'ac-complete-mode-map
 `(("<return>"   nil)
   ("RET"        nil)
   ("M-j"        ac-complete)))


(defun auto-complete-settings ()
  "Settings for `auto-complete'."
  (setq help-xref-following nil)

  (add-to-list 'ac-dictionary-directories
               (concat (current-directory) "dict/"))

  (setq ac-auto-show-menu t
        ac-auto-show-menu-immediately-on-auto-complete t
        ac-auto-start t
        ac-dwim t
        ac-candidate-limit ac-menu-height
        ac-quick-help-delay .5
        ac-disable-faces nil)

  (set-default 'ac-sources
               '(ac-source-semantic-raw
                 ac-source-yasnippet
                 ac-source-dictionary
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-imenu
                 ac-source-files-in-current-dir
                 ac-source-filename))

  (dolist (mode ac+-modes)
    (add-to-list 'ac-modes mode))

  (defun ac-start-use-sources (sources)
    (interactive)
    (let ((ac-sources sources))
      (call-interactively 'ac-start))))

(eval-after-load "auto-complete"
  '(auto-complete-settings))

(defun ac-settings-4-cc ()
  "`auto-complete' settings for `cc-mode'."
  (dolist (command `(c-electric-backspace
                     c-electric-backspace-kill))
    (add-to-list 'ac-trigger-commands-on-completing command)))

(eval-after-load "cc-mode"
  '(ac-settings-4-cc))

(defun ac-settings-4-autopair ()
  "`auto-complete' settings for `autopair'."
  (defun ac-trigger-command-p (command)
    "Return non-nil if `this-command' is a trigger command."
    (or
     (and
      (symbolp command)
      (or (memq command ac-trigger-commands)
          (string-match "self-insert-command" (symbol-name command))
          (string-match "electric" (symbol-name command))
          (let* ((autopair-emulation-alist nil)
                 (key (this-single-command-keys))
                 (beyond-autopair (or (key-binding key)
                                      (key-binding (lookup-key local-function-key-map key)))))
            (or
             (memq beyond-autopair ac-trigger-commands)
             (and ac-completing
                  (memq beyond-autopair ac-trigger-commands-on-completing)))))))))

(eval-after-load "autopair"
  '(ac-settings-4-autopair))

(defun ac-settings-4-java ()
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-c ()
  (setq ac-sources
        '(ac-source-libc
          ac-source-dictionary
          ac-source-yasnippet
          ac-source-semantic
          ac-source-apue
          ac-source-unp
          ac-source-liblw
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-cpp ()
  (setq ac-sources
        '(ac-source-libc
          ac-source-libcpp
          ac-source-yasnippet
          ac-source-dictionary
          ac-source-semantic
          ac-source-apue
          ac-source-unp
          ac-source-libboost
          ac-source-liblw
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-text ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-imenu))
  (ac-ispell-ac-setup))

(defun ac-settings-4-eshell ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-symbols
          ac-source-imenu)))

(defun ac-settings-4-ruby ()
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-html ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-tcl ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-awk ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(apply-args-list-to-fun
 (lambda (hook fun)
   (am-add-hooks hook fun))
 `(('c-mode-hook      'ac-settings-4-c)
   ('c++-mode-hook    'ac-settings-4-cpp)
   ('java-mode-hook   'ac-settings-4-java)
   ('text-mode-hook   'ac-settings-4-text)
   ('eshell-mode-hook 'ac-settings-4-eshell)
   ('ruby-mode-hook   'ac-settings-4-ruby)
   ('racer-mode-hook  'ac-racer-setup)
   ('html-mode-hook   'ac-settings-4-html)
   ('awk-mode-hook    'ac-settings-4-awk)
   ('tcl-mode-hook    'ac-settings-4-tcl)))


(provide 'auto-complete-settings)

;;; auto-complete-settings.el ends here
