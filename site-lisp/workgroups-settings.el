;; -*- Emacs-Lisp -*-
;; Settings for `workgroups'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-10-25 15:40>

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

  (workgroups-mode 1)
  (setq wg-prefix-key (kbd "C-c w"))
  (if (null (wg-workgroup-list))
      (wg-create-workgroup "wg-default"))
  (wg-save-session-on-emacs-exit)
  )


(eval-after-load "workgroups2"
  `(workgroups-settings))


(provide 'workgroups-settings)
