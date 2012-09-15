;; -*- Emacs-Lisp -*-
;; Settings for `shell-mode' and `term-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-01-30 01:10>

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
;; Settings for `shell-mode' and `term-mode'
;; Different from `sh-mode', which is mainly for editing shell script,
;; they are for interacting with an inferior shell.
;;

;;; Code:


;; turn on color
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)


(defun shell-mode-settings ()
  "Settings for `shell-mode' and `term-mode'."

  ;; key binding:
  ;; up and down arrow keys to traverse through the previous commands
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[tab] 'comint-dynamic-complete)
  (setq comint-input-sender 'my-shell-simple-send)

  (ansi-color-for-comint-mode-on)
  ;;(setq ansi-color-for-comint-mode t)
  )

(dolist (mode-hook
         '(shell-mode-hook term-mode-hook))
  (add-hook mode-hook 'shell-mode-settings))


(defun my-shell-simple-send (proc command)
  "set \"clear\" command to clear the entire shell buffer"
  ;; run \"man\" command outside the shell -- disable"
  (cond
   ;; Check for clear command and execute clearing.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer))
   ;; Check for man command and execute it.
   ;;((string-match "^[ \t]*man[ \t]*" command)
   ;; (comint-send-string proc "\n")
   ;;  (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
   ;;  (setq command (replace-regexp-in-string "[ \t]+$" "" command))
   ;;  (message (format "command %s command" command))
   ;;  (funcall 'man command)
   ;;  )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))))


(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel
       process
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
           (kill-buffer (process-buffer proc))))))))


(dolist (mode-hook
         '(gdb-mode-hook term-mode-hook))
  (add-hook mode-hook
            'kill-buffer-when-shell-command-exit))


(provide 'shell-mode-settings)