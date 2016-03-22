;;; google-this-settings.el --- Settings for `google-this'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2016 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:46>

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


(require 'google-this)


(defun google-this-settings ()
  "Settings for `google-this'."

  ;;(global-set-key (kbd "C-x g") 'google-this-mode-submap)
  )


(eval-after-load "google-this"
  `(google-this-settings))

;; turn on `google-this'
(google-this-mode 1)


(provide 'google-this-settings)

;;; google-this-settings.el ends here
