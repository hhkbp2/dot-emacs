;; -*- Emacs-Lisp -*-
;; Settings for `workgroups'.

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-09-21 13:12>

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


(require 'workgroups)


(defun workgroups-settings ()
  "Settings for `workgroups'."

  ;; TODO
  (setq wg-prefix-key (kbd "C-c w"))
  ;; (wg-save-session-on-emacs-exit)
  (workgroups-mode 1)
  (if (null (wg-workgroup-list))
      (wg-create-workgroup "wg-default"))
  )


(eval-after-load "workgroups"
  `(workgroups-settings))


(provide 'workgroups-settings)
