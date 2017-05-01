;;; minibuffer-settings.el --- Settings for `minibuffer'

;;; Commentary:
;;
;; `minibuffer'
;; 状态栏下面的辅助输入区

;;; Code:


(use-package minibuffer
  :defer t
  :config
  (progn
    ;; 默认启用minibuffer
    (minibuffer-electric-default-mode t)

    ;; 允许minibuffer自由变化宽度
    (setq resize-mini-windows t)

    ;; recursive commands in minibuffer
    (setq enable-recursive-minibuffers t)

    ;; 在minibuffer里启用icomplete mode，自动补全函数和变量
    (icomplete-mode t)

    ;; 启用部分补全功能，如输入M-x q r r相当于M-x query-replace-regexp
    (if (>= emacs-major-version 24)
        (progn
          (setq completion-styles '(partial-completion initials))
          (setq completion-pcm-complete-word-inserts-delimiters t))
      (partial-completion-mode t))

    ;; `completion-ignored-extensions': 字符串列表，常为后缀
    ;; 补全文件名时不把以其中字符串结尾的文件名列为侯选
    (dolist (file-postfix
             '(;; tarball
               ".bz2" ".7z" ".zip" ".gz" ".tar"
               ;; specified doc
               ".ps" ".pdf" ".chm" ".doc" ".deb"
               ;; picture
               ".png" ".jpg" ".JPG" ".bmp" ".BMP"
               ;; font
               ".ttf"
               ;; directory
               ".hg/" ".svn/" ".git"
               ;; compiled object and temporary file
               ".elc" ; elisp
               ".pyc" ; python
               ".beam" ".boot" ; erlang
               ))
      (add-to-list 'completion-ignored-extensions file-postfix))

    ;; 当寻找一个同名的文件，自动关联上那个文件
    (setq uniquify-buffer-name-style 'forward))
  )

(provide 'minibuffer-settings)

;;; minibuffer-settings.el ends here
