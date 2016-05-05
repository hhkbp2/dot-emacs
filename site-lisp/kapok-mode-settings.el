;;; kapok-mode-settings.el --- Settings for the `kapok-mode'
;; -*- Emacs-Lisp -*-

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-05-05 16:39>

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


(require 'kapok-mode)

(defun kapok-mode-settings ()
  "Settings for `kapok-mode'."

  )

(eval-after-load "kapok-mode"
  `(kapok-mode-settings))

(provide 'kapok-mode-settings)

;;; kapok-mode-settings.el ends here
