;;; dash-at-point-settings.el --- Settings for `dash-at-point'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2015 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:06>

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

(require 'dash-at-point)

(defun dash-at-point-settings ()
  "Settings for `dash-at-point'."

  (global-set-key [(control c) (d)] 'dash-at-point)
  )


(eval-after-load "dash-at-point"
  `(dash-at-point-settings))


(provide 'dash-at-point-settings)

;;; dash-at-point-settings.el ends here
