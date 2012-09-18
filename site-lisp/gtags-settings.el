;; -*- Emacs-Lisp -*-
;; Settings for `gtags-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-09-18 10:15>

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


(autoload 'gtags-mode "gtags")
(require 'dev-base)
(require 'linum-settings)
(require 'dw-functionals)


(defadvice gtags-select-mode (after gtags-select-mode-linum-on)
  "Settings for `gtags-select-mode'."
  (linum-mode 1)
  ;; key-bindings
  (define-key gtags-mode-map [(meta ,)] 'gtags-pop-stack)
  (define-key gtags-mode-map [(meta .)] 'gtags-select-tag))


(defvar dw-office-repository-path-prefix-list
  '("/home" "/data" "/data1" "/data2")
  "The possible path prefix list of office machine repository.")


(defun file-real-directory-p (file-name)
  "Return t if `filename' names an existing real directory, not symbolic link."
  (and (not (file-symlink-p file-name))
       (file-directory-p file-name)))


(defun expand-directory-name (name &optional default-directory)
  "Convert filename `name' to absolute, if it exist as directory."
  (let ((dir-name (file-truename name)))
    (if (file-directory-p dir-name)
        (expand-file-name dir-name))))


(defun dw-get-real-dir (prefix-list)
  "Return gtags root directory on office machine."
  (when prefix-list
    (let* ((prefix (first prefix-list))
           (dir (expand-file-name
                 (concat (user-login-name) "/rep/trunk") prefix)))
      (if (file-real-directory-p dir)
          dir
        (dw-get-real-dir (rest prefix-list))))))


(defun dw-guess-gtags-root-dir ()
  "Return gtags root directory on current machine, a guess result."
  (if (dw-on-office-machine)
      (dw-get-real-dir dw-office-repository-path-prefix-list)
    (expand-directory-name "~/program")))


(defun gtags-settings ()
  "Settings for `gtags-mode'."

  (let ((dir (dw-guess-gtags-root-dir)))
    (if dir
        (setq gtags-rootdir dir)))

  ;; set code path display style in `gtags-select-mode'
  ;; alternative values: root, relative, absolute
  (setq gtags-path-style 'root)

  ;; activate advices
  (ad-activate 'gtags-select-mode)

  ;; key-bindings
  (define-key gtags-mode-map [(meta ,)] 'gtags-pop-stack)
  (define-key gtags-mode-map [(meta .)] 'gtags-find-tag-from-here))


(eval-after-load "gtags-mode"
  `(gtags-settings))


(dolist (mode-hook dev-mode-hook-list-static)
  (add-hook mode-hook
            '(lambda ()
               (gtags-mode 1)
               (gtags-settings))))


(provide 'gtags-settings)