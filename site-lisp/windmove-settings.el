;;; windmove-settings.el --- Settings for `windmove'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'windmove)


(defun windmove-settings ()
  "Settings for `windmove'."

  ;; 把默认的windows move键modifier由`shift'换成`meta'
  ;; 用 `modifier-{left,right,up,down}'可以在打开的窗口中的跳转
  (windmove-default-keybindings 'meta)
  )

(eval-after-load "windmove"
  `(windmove-settings))


(provide 'windmove-settings)

;;; windmove-settings.el ends here
