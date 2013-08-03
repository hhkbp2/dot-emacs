;; -*- Emacs-Lisp -*-
;; Settings for `mode-line'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-07-31 17:16>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


;; 在mode-line上用彩色显示当前buffer行数
(defun get-lines-4-mode-line ()
  (let ((lines (count-lines (point-min) (point-max))))
    (concat (propertize
             (format "%dL" lines)
             'mouse-face 'mode-line-highlight
             ;; 加上颜色
             'face 'mode-line-lines-face
             'help-echo (format "%d lines" lines)))))

(defun get-size-indication-format ()
  (if (and transient-mark-mode mark-active)
      (format "%dL %dC" (count-lines (region-beginning) (region-end))
              (abs (- (mark t) (point))))
    "%I"))

(defun get-mode-line-region-face ()
  (and transient-mark-mode mark-active
       (if window-system 'region 'region-invert)))


(defun mode-line-settings ()
  "Settings for `mode-line'."

  ;; 在状态栏显示行号
  (line-number-mode 1)
  ;; 在状态栏显示列号
  (column-number-mode 1)

  ;; 在状态栏显示日期时间
  (setq display-time-day-and-date t)
  ;; 设置时间显示为24小时制
  (setq display-time-24hr-format t)
  ;; 设置时间显示格式为:
  ;; <hour>:<minute> <month>-<day> <weekday>
  (setq display-time-format "<%H:%M %b %d %A>")
  ;; 设置时间显示更新间隔(单位: 秒)
  (setq display-time-interval 20)

  ;; 在状态行显示时间
  ;;(display-time)

  (when (display-graphic-p)
    (copy-face 'region 'region-invert)
    (invert-face 'region-invert))

  (size-indication-mode -1)
  (setq-default mode-line-buffer-identification
                (propertized-buffer-identification "%15b"))

  (if is-after-emacs-23
      (setq-default
       mode-line-position
       `((:eval
          ;; 当显示行号已经打开时, 则不在mode-line上显示行号
          (if line-number-mode
              (if column-number-mode
                  (propertize
                   "(%l,%c) "
                   'local-map mode-line-column-line-number-mode-map
                   'mouse-face 'mode-line-highlight
                   'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")
                (propertize
                 "L%l "
                 'local-map mode-line-column-line-number-mode-map
                 'mouse-face 'mode-line-highlight
                 'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu"))
            (if column-number-mode
                (propertize
                 "C%c "
                 'local-map mode-line-column-line-number-mode-map
                 'mouse-face 'mode-line-highlight
                 'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu"))))
         ;;        (:propertize
         ;;         "%p"
         ;;         'local-map mode-line-column-line-number-mode-map
         ;;         'mouse-face 'mode-line-highlight
         ;;         'help-echo "Size indication mode\n\
         ;; mouse-1: Display Line and Column Mode Menu")
         (:eval (get-lines-4-mode-line))
         ;; 当选中一块区域后, 会高亮显示这个区域有多少个字符, 没有选中区域的时候, 则显示当前buffer的大小
         (size-indication-mode
          (" "
           (:eval
            (propertize (get-size-indication-format)
                        'face (and transient-mark-mode mark-active (get-mode-line-region-face))
                        'local-map mode-line-column-line-number-mode-map
                        'mouse-face 'mode-line-highlight
                        'help-echo "Buffer position, mouse-1: Line/col menu"))))))
    (let* ((help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
      (setq-default
       mode-line-position
       `((:eval
          (if (and line-number-mode (not linum-mode))
              (if column-number-mode
                  (propertize "(%l,%c) " 'help-echo ,help-echo)
                (propertize "L%l " 'help-echo ,help-echo))
            (if column-number-mode
                (propertize "C%c " 'help-echo ,help-echo))))
         ;; (:propertize "%p" 'help-echo ,help-echo)
         (:eval (get-lines-4-mode-line))
         (size-indication-mode
          (" " (:eval (propertize
                       (get-size-indication-format) 'help-echo ,help-echo
                       'face (and transient-mark-mode mark-active (get-mode-line-region-face))))))))))

  (let* ((help-echo
          "mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
         (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
         (standard-mode-line-modes
          (list
           " "
           (propertize "%[" 'help-echo recursive-edit-help-echo)
           (propertize "(" 'help-echo help-echo)
           `(:propertize ("" mode-name)
                         help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                         mouse-face mode-line-highlight
                         local-map ,mode-line-major-mode-keymap)
           '("" mode-line-process)
           `(:propertize ("" minor-mode-alist)
                         mouse-face mode-line-highlight
                         help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                         local-map ,mode-line-minor-mode-keymap)
           (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
                       'mouse-face 'mode-line-highlight
                       'local-map (make-mode-line-mouse-map
                                   'mouse-1 #'mode-line-widen))
           (propertize ")" 'help-echo help-echo)
           (propertize "%]" 'help-echo recursive-edit-help-echo))))
    (setq-default mode-line-modes standard-mode-line-modes)
    (setq-default mode-line-format
                  `("-%e"
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    " "
                    mode-line-buffer-identification
                    ,(propertize " " 'help-echo help-echo)
                    mode-line-position
                    (vc-mode vc-mode)
                    mode-line-modes
                    (which-func-mode (" " which-func-format))
                    (display-time-string (" " display-time-string))
                    (working-mode-line-message (" " working-mode-line-message))
                    ,(propertize "-%-" 'help-echo help-echo))))

  (setq mode-line-format-bak mode-line-format)
  (setq mode-line t)

  ;; 在标题栏显示登陆名称和文件名
  (setq frame-title-format
      '("Emacs - "
        (:eval (or (buffer-file-name) (buffer-name)))))
  )

(mode-line-settings)


(defun toggle-mode-line ()
  "Toggle mode-line."
  (interactive)
  (if mode-line
      (setq-default mode-line-format nil)
    (setq-default mode-line-format mode-line-format-bak))
  (setq mode-line (not mode-line)))


(provide 'mode-line-settings)
