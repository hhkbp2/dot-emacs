;;; highlight-tail-settings.el --- Settings for `highlight-tail-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `highlight-tail-mode'
;; 高亮标记最近的修改

;;; Code:


(require 'highlight-tail)

(require 'dev-base-settings)


(defun highlight-tail-settings ()
  "Settings for `highlight-tail-mode'."

  ;; 设置highlight-tail-colors控制渐变颜色,
  ;; a. 适合白底黑字
  ;;(setq highlight-tail-colors
  ;;        '(("#c1e156" . 0)
  ;;          ("#b8ff07" . 25)
  ;;          ("#00c377" . 60)))
  ;; b. 适合黑底白字
  ;; b1. 单色
  ;; (setq highlight-tail-colors
  ;;           '(("black" . 0)
  ;;             ("#bc2525" . 25)
  ;;             ("black" . 66)))
  ;; b2. 混色
  (setq highlight-tail-colors
        '(("black" . 0)
          ("red" . 40)
          ("blue" . 80)
          ("black" . 100)))
  ;; b3. 华丽，适合黑底白字
  ;; (setq highlight-tail-colors
  ;;       '(("black" . 0)
  ;;         ("green" . 20)
  ;;         ("blue" . 40)
  ;;         ("yellow" . 60)
  ;;         ("red" . 80)))
  )

(eval-after-load "highlight-tail"
  `(highlight-tail-settings))


;; 自动加载
(dolist (mode-hook dev-mode-hook-list)
  (add-hook mode-hook
            'highlight-tail-mode))


(provide 'highlight-tail-settings)

;;; highlight-tail-settings.el ends here
