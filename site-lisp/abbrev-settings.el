;;; abbrev-settings.el --- Settings for `abbrev-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defun abbrev-settings ()
  "Settings for `abbrev-mode'."

  ;; 默认打开缩写模式
  (setq-default abbrev-mode t)
  ;; 设置缩写定义文件
  (read-abbrev-file "~/.emacs.d/abbrev_defs")
  ;; 设置自动保存缩写定义文件
  (setq save-abbrevs t))


(eval-after-load "abbrev-mode"
  `(abbrev-settings))


(provide 'abbrev-settings)

;;; abbrev-settings.el ends here
