;;; hs-minor-mode-settings.el --- Settings for `hs-minor-mode'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:53>

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
;;
;; `hs-minor-mode':
;; fold and unfold code

;;; Code:


(require 'hideshow)
(require 'eval-after-load)
(require 'util)


(defun hs-minor-mode-settings ()
  "Settings for `hs-minor-mode'."

  (setq hs-isearch-open t)

  (setq-default mode-line-format
                (append '((:eval (hs-display-headline))) mode-line-format))

  (setq hs-set-up-overlay 'hs-abstract-overlay)

  ;; key bindings
  (define-key hs-minor-mode-map [(control c) (\,) (H)] 'hs-hide-all)
  (define-key hs-minor-mode-map [(control c) (\,) (S)] 'hs-show-all)
  (define-key hs-minor-mode-map [(control c) (\,) (h)] 'hs-hide-block)
  (define-key hs-minor-mode-map [(control c) (\,) (s)] 'hs-show-block)
  (define-key hs-minor-mode-map [(control c) (\,) (l)] 'hs-hide-level)
  (define-key hs-minor-mode-map [(control c) (\,) (t)] 'hs-toggle-hiding)

  ;; use mouse to hide/show
  (define-key hs-minor-mode-map [(C-down-mouse-1)] 'hs-toggle-hiding)

  ;; (define-key-list hs-minor-mode-map
  ;;   '(("C-c h" hs-hide-block)
  ;;     ("C-c H" hs-hide-all)
  ;;     ("C-c s" hs-show-block)
  ;;     ("C-c S" hs-show-all)))
  )

(eval-after-load "hideshow"
  '(hs-minor-mode-settings))


(defvar hs-headline-max-len 30
  "*Maximum length of `hs-headline' to display.")


(defun hs-display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs-headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs-headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))


(defun hs-abstract-overlay (ov)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (str (format "<%d lines>" (count-lines start end))) text)
    (setq text
          (propertize str
                      'face 'hs-block-flag-face 'help-echo
                      (buffer-substring (1+ start) end)))
    (overlay-put ov 'display text)
    (overlay-put ov 'pointer 'hand)
    (overlay-put ov 'keymap hs-overlay-map)))


(defvar hs-hide-all nil "Current state of hideshow for toggling all.")
(make-local-variable 'hs-hide-all)

(defvar fold-all-fun nil "Function to fold all.")
(make-variable-buffer-local 'fold-all-fun)

(defvar fold-fun nil "Function to fold.")
(make-variable-buffer-local 'fold-fun)


(defun hs-toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq hs-hide-all (not hs-hide-all))
  (if hs-hide-all
      (hs-hide-all)
    (hs-show-all)))


(defun toggle-fold-all ()
  "Toggle fold all."
  (interactive)
  (if fold-all-fun
      (call-interactively fold-all-fun)
    (hs-toggle-hiding-all)))


(defun toggle-fold ()
  "Toggle fold."
  (interactive)
  (if fold-fun
      (call-interactively fold-fun)
    (hs-toggle-hiding)))


(defun hs-minor-mode-4-emaci-settings ()
  "Settings of `hs-minor-mode' for `emaci'."
  (eal-define-keys
   'emaci-mode-map
   '(("s" toggle-fold)
     ("S" toggle-fold-all))))


(defvar hs-overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

;; (eal-define-keys-commonly hs-overlay-map
;;                           '(("<mouse-1>" hs-show-block)))


(eval-after-load "emaci"
  '(hs-minor-mode-4-emaci-settings))


(provide 'hs-minor-mode-settings)

;;; hs-minor-mode-settings.el ends here
