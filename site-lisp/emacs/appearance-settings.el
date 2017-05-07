;;; appearance-settings.el --- Settings for appearance

;;; Commentary:

;;; Code:


(require 'font-lock-settings)
(require 'basic-faces-settings)
(require 'frame-settings)


(defun auto-scrolling-settings ()
  "Settings for auto scrolling."

  ;; make scroll movement more comfortable
  (setq
   ;; scroll one line up or down every time
   scroll-step 1
   ;; display just one line at the margins of top and bottom
   scroll-margin 1
   ;; always recenter pont if it moves off screen.
   scroll-conservatively 0)
  ;; don't scroll automatically to recenter the point in display
  (setq-default scroll-up-aggressively 0
                scroll-down-aggressively 0))


(defun encoding-settings ()
  "Settings for encoding."
  (let ((prefer-coding-list '(utf-8 gbk gb2312 big5)))
    (dolist (coding (reverse prefer-coding-list))
      (prefer-coding-system coding)))
  (setq locale-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (set-language-environment-coding-systems "utf-8"))


(defun appearance-settings ()
  "Settings for appearance."

  (frame-settings)

  ;; close startup message
  (setq inhibit-startup-message t)

  (if (display-graphic-p)
      ;; 用滚轴鼠标
      (mouse-wheel-mode t))

  (auto-scrolling-settings)

  ;; don't blink the cursor
  (blink-cursor-mode nil)

  ;; mouse will jump away if the cursor is near it
  (mouse-avoidance-mode 'animate)

  ;; highlight marked region
  (setq-default transient-mark-mode t)

  ;; split the window horizontally when use ediff
  (setq ediff-split-window-function 'split-window-horizontally)

  (when (>= emacs-major-version 24)
    ;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
    (setq x-select-enable-clipboard t)
    ;; after mouse selection in X11, you can paste by `yank' in emacs
    (setq x-select-enable-primary t)
    )

  (encoding-settings)

  ;; 加载font-lock配置
  (font-lock-settings)

  ;; 设置背景景色为暗色以配合主题
  ;;(custom-set-variables '(frame-background-mode (quote dark)))

  ;; load `color-theme' settings
  (require 'color-theme-settings)
  ;; load my favorite color theme
  (require 'color-theme-darkmate)
  (color-theme-darkmate)
  )

(appearance-settings)

(provide 'appearance-settings)

;;; appearance-settings.el ends here
