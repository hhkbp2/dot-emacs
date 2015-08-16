;; -*- Emacs-Lisp -*-
;; Settings for `zeal-at-point'.

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-08-16 18:30>

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

(require 'zeal-at-point)

(defun zeal-at-point-settings ()
  "Settings for `zeal-at-point'."

  (global-set-key [(control c) (d)] 'zeal-at-point)
  )


(eval-after-load "zeal-at-point"
  `(zeal-at-point-settings))


(provide 'zeal-at-point-settings)

