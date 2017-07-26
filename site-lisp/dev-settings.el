;;; dev-settings.el --- Settings for software development
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'dev-base-settings)
(require 'dw-functionals)


;;; 配置括号匹配高亮与补全
(require 'smartparens-settings)


;;; 去tab化
(defun dw-untabify()
  "Replace TAB with whitespace."
  (add-hook 'write-contents-functions
            '(lambda()
               (save-excursion
                 (untabify (point-min) (point-max)))
               ;; Return nil for the benefit of `write-contents-functions'.
               nil)))

(defun dw-untabify-settings ()
  "Enable `untabify' in specified modes."

  (let* ((excluding-list
          '(;; no untabify in makefile since it uses tab to start commands
            makefile-mode-hook
            ;; no untabify in go source file since
            ;; go officially recommands tab for indentation
            go-mode-hook
            ;; markdown file could contains codes of any language, including
            ;; makefile or go.
            markdown-mode-hook))
         (hook-list (cl-set-difference dev-mode-hook-list excluding-list)))
    (dolist (mode-hook hook-list)
      (add-hook mode-hook 'dw-untabify))))

(dw-untabify-settings)

;;; 删除行末空白
(defun dw-delete-trailing-space()
  "Delete the trailing whitespace."
  (add-hook
   ;; buffer local, would be changed after buffer mode change.
   'write-contents-functions
   ;; alternative, buffer local, marked as a permanent-local,
   ;; changing the major mode does not alter it.
   ;; 'write-file-functions
   'delete-trailing-whitespace))

(defun dw-delete-trailing-space-settings ()
  (let* ((excluding-list '(;; don't delete trailing spaces in markdown files
                          ;; (which could represent linefeed)
                          markdown-mode-hook))
        (hook-list (cl-set-difference dev-mode-hook-list excluding-list)))
  (dolist (mode-hook hook-list)
    (add-hook mode-hook 'dw-delete-trailing-space))))

(dw-delete-trailing-space-settings)

(when (dw-version->=-p 24 3)             ; since version 24.3
  ;; don't delete trailing empty lines of buffer
  (setq delete-trailing-lines nil))


(defconst dw-tab-width 4)

(defun dw-tab-settings ()
  "Settings for tab in various file formats."

  ;; 用空格而不用tab来对齐(默认用tab来对齐)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence dw-tab-width 120 dw-tab-width))

  (dolist (mode-hook dev-mode-hook-list)
    (add-hook mode-hook
              '(lambda ()
                 ;; set tab width
                 (setq tab-width dw-tab-width)))))

(dw-tab-settings)


(require 'occur-settings)

(require 'emacs-lisp-mode-settings)
(require 'common-lisp-mode-settings)
(require 'scheme-mode-settings)
(require 'joxa-mode-settings)
(require 'kapok-mode-settings)

(require 'wlc-settings)
(require 'rainbow-delimiters-settings)

(require 'erlang-mode-settings)

(require 'clojure-mode-settings)

(require 'ruby-mode-settings)
(require 'python-mode-settings)

(require 'go-mode-settings)

(require 'cc-mode-settings)

(require 'makefile-mode-settings)

(require 'sh-mode-settings)
(require 'awk-mode-settings)
(require 'conf-mode-settings)

(require 'shell-mode-settings)
(require 'multi-term-settings)

(require 'thrift-mode-settings)
(require 'protobuf-mode-settings)

(require 'markdown-mode-settings)
(require 'scss-mode-settings)


;; 高亮光标处单词
(require 'highlight-symbol-settings)

(defun dw-highlight-symbol-settings ()
  (dolist (mode-hook dev-mode-hook-list)
    (add-hook mode-hook 'highlight-symbol-mode-on)))
(dw-highlight-symbol-settings)

;; 高亮最新做的修改
;;(require 'highlight-tail-settings)

;; 高亮当前行
(require 'highlight-current-line-settings)

;; 高亮缩进
(require 'highlight-indentation-settings)

;; hs-minor-mode,折叠代码
(require 'hs-minor-mode-settings)

(defun dw-hs-minor-mode-settings()
  (dolist (mode-hook dev-mode-hook-list)
    (add-hook mode-hook 'hs-minor-mode)))
(dw-hs-minor-mode-settings)

(require 'whitespace-settings)

;; flycheck
(require 'flycheck-settings)

;; neotree
(require 'neotree-settings)

;; global
(require 'gtags-settings)

;; 所有的自动补全的配置
(require 'yasnippet-settings)
(require 'auto-complete-settings-all)


(provide 'dev-settings)

;;; dev-settings.el ends here
