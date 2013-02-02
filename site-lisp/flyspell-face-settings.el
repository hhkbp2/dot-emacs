;; -*- Emacs-Lisp -*-
;; Face settings for `flyspell-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2011-02-05 15:24>

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


(defun flyspell-face-settings ()
  "Face settings for `flyspell-mode'."
  (custom-set-faces
   '(flyspell-duplicate ((t (:foreground "#fcaf3e" :underline t))))
   '(flyspell-incorrect ((t (:foreground "#cc0000" :strike-through t)))))
  )


(eval-after-load "flyspell"
  '(flyspell-face-settings))


(provide 'flyspell-face-settings)