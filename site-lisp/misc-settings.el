;;; misc-settings.el --- Miscellaneous settings
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'popwin)
(require 'zone-settings)


(defun misc-settings ()
  "Miscellaneous settings."

  ;; 个人信息
  (setq user-full-name "Dylan.Wen")
  (setq user-mail-address "hhkbp2@gmail.com")


  ;; 默认主模式用 text-mode
  (setq default-major-mode 'text-mode)

  ;; 开启text-mode的时候，同时开启自动填充模式
  ;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; 设置默认的自动换行长度
  (setq default-fill-column 80)

  ;; disable auto-backup
  (setq make-backup-files nil)

  ;; 设置自动保存的击键次数，即每隔100次击键就自动保存文件
  (setq auto-save-interval 100)

  ;; Save auto-save files to the system's temporary directory
  ;; to avoid they from cluttering my file tree.
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; automatically save modified file-visiting buffer without query while exit
  (add-hook 'find-file-hook '(lambda ()
                              (setq buffer-save-without-query t)))

  ;; set kill-ring size
  (setq kill-ring-max 20)

  ;; share the x window clipboard (windows user don't need this)
  (setq x-select-enable-clipboard t)
  ;; 支持中键粘贴
  (setq mouse-yank-at-point t)

  ;; use y or n instead of yes or no answer
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; 打开命令的快捷键提示
  (setq suggest-key-bindings t)

  ;; 在emacs读man文档时，使用当前buffer
  (setq Man-notify-method 'pushy)

  ;; display images
  (setq auto-image-file-mode t)

  ;; enable some useful functions
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'scroll-left 'disabled nil)

  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  ;; turn on `popwin'
  (popwin-mode 1)

  ;; apply `zone' settings
  (zone-settings)
  )

(misc-settings)


(provide 'misc-settings)

;;; misc-settings.el ends here
