;; -*- Emacs-Lisp -*-
;; Settings for `workgroups'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-10-25 16:22>

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

;;; Code:


(require 'workgroups2)


(defun workgroups-settings ()
  "Settings for `workgroups'."

  (setq wg-prefix-key (kbd "C-c w"))
  (setq wg-session-file "~/.emacs.d/emacs_workgroups")
  (workgroups-mode 1)

  ;; What to do on Emacs exit / workgroups-mode exit?
  (setq wg-emacs-exit-save-behavior 'save) ; Options: 'save 'ask nil
  (setq wg-workgroups-mode-exit-save-behavior 'save) ; Options: 'save 'ask nil

  ;; Mode Line changes
  ;; Display workgroups in Mode Line?
  ;; (setq wg-mode-line-display-on t) ; Default: (not (featurep 'powerline))
  (setq wg-flag-modified t)
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]" ; how to surround it
        wg-mode-line-decor-divider ":")
  )


(eval-after-load "workgroups2"
  `(workgroups-settings))


(provide 'workgroups-settings)
