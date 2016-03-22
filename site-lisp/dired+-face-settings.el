;;; dired+-face-settings.el --- Face settings for `dired+'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

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


(defun dired+-face-settings ()
  "Face settings for `dired+'."
  (custom-set-faces
   '(diredp-compressed-file-suffix
     ((((class color) (min-colors 88)) (:foreground "#fce94f"))
       (((class color) (min-colors 16)) (:foreground "yellow"))
       (((class color) (min-colors 8)) (:foreground "yellow"))
       (((type tty) (class mono)) (:foreground "yellow"))
       (t (:foreground "#fce94f"))))
   '(diredp-date-time
     ((((class color) (min-colors 88)) (:foreground "#00c900" :slant italic))
      (((class color) (min-colors 16)) (:foreground "#00c900" :slant italic))
      (((class color) (min-colors 8)) (:foreground "green" :slant italic))
      (((type tty) (class mono)) (:foreground "green" :slant italic))
      (t (:foreground "#00c900" :slant italic))))
   '(diredp-deletion
     ((((class color) (min-colors 88)) (:foreground "#ff2f6a" :weight bold))
      (((class color) (min-colors 16)) (:foreground "red" :weight bold))
      (((class color) (min-colors 8)) (:foreground "red" :weight bold))
      (((type tty) (class mono)) (:foreground "red" :weight bold))
      (t (:foreground "#ff2f6a" :weight bold))))
   '(diredp-deletion-file-name
     ((((class color) (min-colors 88)) (:foreground "#ff2f6a" :weight bold))
      (((class color) (min-colors 16)) (:foreground "red" :weight bold))
      (((class color) (min-colors 8)) (:foreground "red" :weight bold))
      (((type tty) (class mono)) (:foreground "red" :weight bold))
      (t (:foreground "#ff2f6a" :weight bold))))
   '(diredp-dir-heading
     ((((class color) (min-colors 88))
       (:foreground "#bb66ff" :weight bold))
      (((class color) (min-colors 16))
       (:foreground "purple" :weight bold))
      (((class color) (min-colors 8))
       (:foreground "purple" :weight bold))
      (((type tty) (class mono))
       (:foreground "purple" :weight bold))
      (t (:foreground "#bb66ff" :weight bold))))
   '(diredp-dir-name
     ((((class color) (min-colors 88)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 16)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
      (((type tty) (class mono)) (:foreground "blue" :weight bold))
      (t (:foreground "#9e91ff" :weight bold))))
   '(diredp-dir-priv
     ((((class color) (min-colors 88)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 16)) (:foreground "#9e91ff" :weight bold))
      (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
      (((type tty) (class mono)) (:foreground "blue" :weight bold))
      (t (:foreground "#9e91ff" :weight bold))))
   '(diredp-display-msg
     ((t (nil))))
   '(diredp-executable-tag
     ((((class color) (min-colors 88)) (:foreground "green3"))
      (((class color) (min-colors 16)) (:foreground "green3"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (((type tty) (class mono)) (:foreground "green"))
      (t (:foreground "green3"))))
   '(diredp-exec-priv
     ((((class color) (min-colors 88)) (:foreground "#fce94f" :slant italic))
      (((class color) (min-colors 16)) (:foreground "#fce94f" :slant italic))
      (((class color) (min-colors 8)) (:foreground "yellow" :slant italic))
      (((type tty) (class mono)) (:foreground "yellow" :slant italic))
      (t (:foreground "#fce94f" :slant italic))))
   '(diredp-file-name
     ((((class color) (min-colors 88)) (:foreground "#eeeeee"))
      (((class color) (min-colors 16)) (:foreground "#eeeeee"))
      (((class color) (min-colors 8)) (:foreground "white"))
      (((type tty) (class mono)) (:foreground "white"))
      (t (:foreground "#eeeeee"))))
   '(diredp-file-suffix
     ((t (nil))))
   '(diredp-flag-mark
     ((((class color) (min-colors 88)) (:foreground "#009cff"))
      (((class color) (min-colors 16)) (:foreground "#009cff"))
      (((class color) (min-colors 8)) (:foreground "cyan"))
      (((type tty) (class mono)) (:foreground "cyan"))
      (t (:foreground "#009cff"))))
   '(diredp-flag-mark-line
     ((((class color) (min-colors 88)) (:foreground "#009cff"))
      (((class color) (min-colors 16)) (:foreground "#009cff"))
      (((class color) (min-colors 8)) (:foreground "cyan"))
      (((type tty) (class mono)) (:foreground "cyan"))
      (t (:foreground "#009cff"))))
   '(diredp-ignored-file-name
     ((((class color) (min-colors 88)) (:foreground "grey70"))
      (((class color) (min-colors 16)) (:foreground "grey70"))
      (((class color) (min-colors 8)) (:foreground "white"))
      (((type tty) (class mono)) (:foreground "white"))
      (t (:foreground "grey70"))))
   '(diredp-link-priv
     ((t (nil))))
   '(diredp-no-priv
     ((t (nil))))
   '(diredp-other-priv
     ((t (nil))))
   '(diredp-rare-priv
     ((t (nil))))
   '(diredp-read-priv
     ((t (nil))))
   '(diredp-symlink
     ((((class color) (min-colors 88)) (:foreground "#bb66ff"))
      (((class color) (min-colors 16)) (:foreground "#bb66ff"))
      (((class color) (min-colors 8)) (:foreground "purple"))
      (((type tty) (class mono)) (:foreground "purple"))
      (t (:foreground "#bb66ff"))))
   '(diredp-write-priv
     ((((class color) (min-colors 88)) (:foreground "#ff44cc"))
      (((class color) (min-colors 16)) (:foreground "#ff44cc"))
      (((class color) (min-colors 8)) (:foreground "red"))
      (((type tty) (class mono)) (:foreground "red"))
      (t (:foreground "#ff44cc")))))
  )


(eval-after-load "dired+"
  '(dired+-face-settings))


(provide 'dired+-face-settings)

;;; dired+-face-settings.el ends here
