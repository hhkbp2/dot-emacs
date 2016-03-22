;;; dired-settings.el --- Settings for `dired'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:10>

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


(require 'wuxch-dired "my-wuxch-dired")
(require 'wuxch-dired-copy-paste "my-wuxch-dired-copy-paste")

(require 'dired-details+)

(defun dired-details-settings ()
  "Settings for `dired-details'."
  ;; hide dired details on dired buffer, default is t
  ;;(setq dired-details-initially-hide nil)
  ;; show symbolic link target path
  (setq dired-details-hide-link-targets nil)
  )

(eval-after-load "dired-details"
  `(dired-details-settings))

(require 'dired+-settings)
(require 'dw-functionals)

(define-key global-map (kbd "C-x d") 'dired-jump)

(defadvice dired-jump (after revert-dired-buffer
                             activate compile)
  "When display dired, revert buffer to see the latest update."
  (revert-buffer))

(defun his-dired-sort ()
  "dired-mode中让目录显示在文件前"
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'his-dired-sort)
(add-hook 'dired-lood-hook 'his-dired-sort)

(defun ywb-dired-filter-regexp (regexp &optional arg)
  "dired mode中只显示后缀名符合正则表达式的文件和目录"
  (interactive
   (list (dired-read-regexp
          (concat (if current-prefix-arg "只显示不" "只显示") "匹配的后缀名(regexp): "))
         current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (or arg (if (fboundp 'dired-do-toggle) (dired-do-toggle) (dired-toggle-marks)))
  (dired-do-kill-lines))

(defun ywb-dired-filter-extension (extension &optional arg)
  "dired mode中只显示后缀名为EXTENSION的文件和目录"
  (interactive
   (list (read-from-minibuffer
          (concat "只显示后缀名为" (if current-prefix-arg "" "") ": "))
         current-prefix-arg))
  (ywb-dired-filter-regexp (concat "\\." extension "\\'") arg))

(defvar ywb-dired-quickview-buffer nil)
(defun ywb-dired-quickview ()
  "类似TC的一个命令,可以使用同一个buffer浏览多个文件，每次打开新文件就把前一个buffer关了"
  (interactive)
  (if (buffer-live-p ywb-dired-quickview-buffer)
      (kill-buffer ywb-dired-quickview-buffer))
  (setq ywb-dired-quickview-buffer
        (find-file-noselect (dired-get-file-for-visit)))
  (display-buffer ywb-dired-quickview-buffer))

;; wdired提供修改文件名的一种非常方便方法。它把dired-mode当作一般的
;; 文本处理，这样无论是修改一个文件，还是批量修改文件都不是一般的爽。
(if is-before-emacs-21 (require 'wdired "wdired-for-21"))

;; 让你能够在dired-mode里面使用只对文件名部分执行i-search
(require 'dired-isearch "my-dired-isearch")

(defun dired-up-directory-same-buffer ()
  "Goto parent directory in the smae buffer."
  (interactive)
  (let* ((dir (dired-current-directory))
         (dir-file-name (directory-file-name dir)))
    (unless (string= dir dir-file-name)
      (find-alternate-file "..")
      (dired-goto-file dir-file-name))))

(defun dired-keys ()
  "dired-mode中的快捷键定义"
  (define-prefix-command 'dired-slash-map)
  (apply-define-key
   dired-mode-map
   `((","           dired-up-directory-same-buffer)
     ("."           diredp-find-file-reuse-dir-buffer)
     ("<backspace>" dired-up-directory-same-buffer)
     ("RET"         diredp-find-file-reuse-dir-buffer)
     ("<return>"    diredp-find-file-reuse-dir-buffer)
     ("<left>"      dired-up-directory-same-buffer)
     ("<right>"     diredp-find-file-reuse-dir-buffer)
     ("<up>"        wuxch-dired-previous-line)
     ("<down>"      wuxch-dired-next-line)
     ("M-p"         wuxch-dired-previous-line)
     ("M-n"         wuxch-dired-next-line)
     ("M-s"         dired-sort-toggle-or-edit)
     ("M-g"         revert-buffer)
     ("M-r"         diredp-rename-this-file)
     ;; ("M-i m"       dired-mark)
     ("M-a"         wuxch-mark-all-files-directories)
     ;; ("M-i u"       dired-unmark)
     ;; ("M-i a"       dired-unmark-all-marks)
     ("DEL"         dired-unmark-backward)
     ("M-o"         dired-omit-mode)
     ("M-/"         dired-undo)
     ("M-Y"         dired-redo)
     ("SPC"         scroll-up)
     ("C-a"         move-beginning-of-line)
     ("C-e"         move-end-of-line)
     ("C-i d"       dired-flag-file-deletion)
     ("C-i k"       dired-do-delete)
     ;;
     ("C-s"         dired-lis-isearch-forward-always?)
     ("C-r"         dired-lis-isearch-backward-always?)
     ("ESC C-s"     dired-lis-isearch-forward-regexp-always?)
     ("ESC C-r"     dired-lis-isearch-backward-regexp-always?)
     ;;
     ("/"           dired-slash-map)
     ("/m"          ywb-dired-filter-regexp)
     ("/."          ywb-dired-filter-extension)
     ("C-q"         ywb-dired-quickview)
     ("M-e"         ywb-dired-quickview)
     ;;
     ;;("C-c C-m"     make-sb)
     ;;("C-c m"       make-check-sb)
     ;;("C-c M"       make-clean-sb)
     ;;("C-c c"       compile-buffer-sb)
     ;;("C-c r"       run-program-sb)
     ;;("C-c C"       smart-compile-sb)
     ;;("C-c g"       gdb)
     ;;("C-c b"       gud-break)
     ;;("C-c B"       gud-remove)
     )))

(defun dired-settings ()
  "Settings for `dired-mode'."
  (dired-keys)
  (setq mode-line-buffer-identification (propertized-buffer-identification "%b"))
  ;;
  ;; added by Dylan.Wen
  (setq dired-listing-switches "-alh"
        ;; "-b" doesn't work well:
        ;; after it's enabled, cannot open file with nongraphic character
        ;; "-F" doesn't work
        ;; after it's enabled, 'dired-isearch-forward' sometimes doesn't work
        dired-ls-F-marks-symlinks nil)
  )

(add-hook 'dired-mode-hook 'dired-settings)

(require 'dired-x-settings)

(defun dired-sort-by-size ()
  "sort by Size"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-by-extension ()
  "sort by eXtension"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-by-time ()
  "sort by Time"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-by-name ()
  "sort by Name"
  (interactive)
  (dired-sort-other dired-listing-switches))

(require 'dired-lis-settings)

(def-redo-command dired-redo 'dired-redo 'dired-undo)

(require 'dired-sort-menu+)


(provide 'dired-settings)

;;; dired-settings.el ends here
