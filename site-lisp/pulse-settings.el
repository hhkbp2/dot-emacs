;; -*- Emacs-Lisp -*-
;; Settings for `pulse'.

;; Copyright (C) 2013 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-01-30 15:17>

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

;; The `pulse' code in `cedet' package integrated in Emacs (after version 23)
;; cuts loose all the advices, which are included in independent `cedet'
;; release. these advices are still useful while I prefer to use the
;; integrated version of `cedet'.


;;; Advices:


(defun pulse-toggle-integration-advice (arg)
  "Toggle activation of advised functions that will now pulse.
Wint no ARG, toggle the pulse advice.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'"
  (interactive "P")
  (if (null arg)
      (setq pulse-command-advice-flag (not pulse-command-advice-flag))
    (if (< (prefix-numeric-value arg) 0)
        (setq pulse-command-advice-flag nil)
      (setq pulse-command-advice-flag t)
      )
    )
  (if pulse-command-advice-flag
      (message "Pulse advice enabled")
    (message "Pulse advice disabled"))
  )

(defadvice goto-line (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag)
    (pulse-momentary-highlight-one-line (point))))

(defadvice exchange-point-and-mark (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (> (abs (- (point) (mark))) 400))
    (pulse-momentary-highlight-one-line (point))))

(defadvice find-tag (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag)
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-search (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag)
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-loop-continue (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag)
    (pulse-momentary-highlight-one-line (point))))

(defadvice pop-tag-mark (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag)
    (pulse-momentary-highlight-one-line (point))))

(defadvice imenu-default-goto-function (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))


;;; Additional advices:

(defadvice exchange-point-and-mark (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p)
             (> (abs (- (point) (mark))) 400))
    (pulse-momentary-highlight-one-line (point))))

(defadvice switch-to-buffer (after pulse-advice activate)
  "Cause the current line of new buffer to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice ido-switch-buffer (after pulse-advice activate)
  "Cause the current line of new buffer to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice previous-buffer (after pulse-advice activate)
  "After previous-buffer, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice next-buffer (after pulse-advice activate)
  "After next-buffer, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice beginning-of-buffer (after pulse-advice activate)
  "Cause the current line of buffer to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))


(defun pulse-settings ()
  "Settings for `pulse'."
  (when (display-graphic-p)
    ;; enable pulse on advices
    (pulse-toggle-integration-advice t)))

(eval-after-load "pulse"
  `(pulse-settings))


(provide 'pulse-settings)
