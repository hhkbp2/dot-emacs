;; -*- Emacs-Lisp -*-
;; Settings for `highlight-indentation'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2015-04-25 18:35>

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


(defun highlight-indentation-face-settings()
  "Face settings for `highlight-indentation'."
  (custom-set-faces
   '(highlight-indentation-face
     ((((type x)) (:background "#333333"))
      (((class color) (min-colors 256)) (:background "#808080"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (t (:background "black"))))
   '(highlight-indentation-current-column-face
     ((((type x)) (:background "#333333"))
      (((class color) (min-colors 256)) (:background "#808080"))
      (((class color) (min-colors 16)) (:background "black"))
      (((class color) (min-colors 8)) (:background "black"))
      (t (:background "black"))))))

(defun highlight-indentation-settings()
  "Settings for `highlight-indentation'."

  (highlight-indentation-face-settings)
  )

(eval-after-load "highlight-indentation"
  `(highlight-indentation-settings))

(dolist (mode-hook '(c-mode-common-hook
                     java-mode-hook
                     enh-ruby-mode-hook
                     python-mode-hook
                     erlang-mode-hook
                     js-mode-hook
                     html-mode-hook
                     conf-mode-hook))
  (add-hook mode-hook
            (lambda () (highlight-indentation-current-column-mode))))


(provide 'highlight-indentation-settings)
