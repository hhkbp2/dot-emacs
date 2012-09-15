;; -*- Emacs-Lisp -*-
;; Settings for basic faces.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 15:35>

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


(defun basic-faces-settings ()
  "Settings for basic faces."
  (custom-set-faces
   '(menu ((t (nil))))
   '(tool-bar ((t (nil))))
   '(scroll-bar ((t (nil))))
   '(mouse ((t (:background "sienna1"))))
   '(cursor
     ((((class color) (min-colors 88)) (:background "#eeeeee"))
      (((class color) (min-colors 16)) (:background "#eeeeee"))
      (((class color) (min-colors 8)) (:background "blue"))
      (((type tty) (class mono)) (:background "blue"))
      (t (:background "#eeeeee"))))
   '(border ((t (:background "black"))))
   '(vertical-border ((t (nil))))
   '(escape-glyph ((t (:foreground "cyan"))))
   '(fixed-pitch ((t (:family "Monospace"))))
   '(variable-pitch ((t (:family "Sans Serif"))))
   '(fringe ((t (:background "grey10"))))
   '(header-line
     ((t (:background "grey20" :foreground "grey90" :weight semi-bold))))
   '(nobreak-space ((t (:foreground "cyan" :underline t))))
   '(shadow
     ((((class color) (min-colors 88)) (:foreground "grey70"))
      (((class color) (min-colors 16)) (:foreground "grey70"))
      (((class color) (min-colors 8)) (nil))
      (((type tty) (class mono)) (nil))
      (t (:foreground "grey70"))))
   '(highlight
     ((((class color) (min-colors 88)) (:background "#222222"))
      (((class color) (min-colors 16)) (:background "#222222"))
      (((class color) (min-colors 8)) (:inverse-video t))
      (((type tty) (class mono)) (:inverse-video t))
      (t (:background "#222222"))))
   '(region
     ((((class color) (min-colors 88)) (:background "#555753"))
      (((class color) (min-colors 16)) (:background "blue3"))
      (((class color) (min-colors 8)) (:background "blue" :foreground "white"))
      (((type tty) (class mono)) (:inverse-video t))
      (t (:background "#555753"))))
   '(region-invert
     ((((class color) (min-colors 88)) (:foreground "#555753"))
      (((class color) (min-colors 16)) (:foreground "blue3"))
      (((class color) (min-colors 8)) (:foreground "blue"))
      (((type tty) (class mono)) (:inverse-video t))
       (t (:foreground "#555753"))))
   '(secondary-selection ((t (:background "#bbbbbb" :foreground "#eeeeee"))))
   '(trailing-whitespace
      ((((class color) (min-colors 88)) (:background "white"))
       (((class color) (min-colors 16)) (:background "white"))
       (((class color) (min-colors 8)) (:background "white"))
       (((type tty) (class mono)) (:background "white"))
       (t (:background "white"))))
   '(link
      ((((class color) (min-colors 88))
        (:foreground "#06989A" :underline t :weight bold))
       (((class color) (min-colors 16))
        (:foreground "#06989A" :underline t :weight bold))
       (((class color) (min-colors 8))
        (:foreground "blue" :underline t :weight bold))
       (((type tty) (class mono))
        (:foreground "blue" :underline t :weight bold))
       (t (:foreground "#06989A" :underline t :weight bold))))
   '(link-visited
      ((((class color) (min-colors 88))
        (:foreground "violet" :underline t :slant italic))
       (((class color) (min-colors 16))
        (:foreground "violet" :underline t :slant italic))
       (((class color) (min-colors 8))
        (:foreground "purple" :underline t :slant italic))
       (((type tty) (class mono))
        (:foreground "purple" :underline t :slant italic))
       (t (:foreground "violet" :underline t :slant italic))))
   '(minibuffer-prompt
      ((((class color) (min-colors 88))
        (:foreground "#fce94f" :weight semi-bold))
       (((class color) (min-colors 16))
        (:foreground "#fce94f" :weight semi-bold))
       (((class color) (min-colors 8))
        (:foreground "yellow" :weight bold))
       (((type tty) (class mono))
        (:foreground "yellow" :weight bold))
       (t (:foreground "#fce94f" :weight semi-bold))))
   '(mode-line
     ((((class color) (min-colors 88))
       (:background "gray10" :foreground "gray60" :weight semi-bold
                    :box (:line-width -1 :color "gray50")))
      (((class color) (min-colors 16))
       (:background "gray10" :foreground "gray60" :weight semi-bold))
      (((class color) (min-colors 8))
       (:background "purple" :foreground "white"))
      (((type tty) (class mono)) (:background "purple" :foreground "white"))
      (t (:background "gray10" :foreground "gray60" :weight semi-bold
                      :box (:line-width -1 :color "gray50")))))
   '(mode-line-buffer-id
     ((((class color) (min-colors 88))
       (:background "gray30" :foreground "#009cff" :weight bold))
      (((class color) (min-colors 16))
       (:background "gray30" :foreground "#009cff" :weight bold))
      (((class color) (min-colors 8))
       (:background "black" :foreground "blue" :weight bold))
      (((type tty) (class mono))
       (:background "black" :foreground "blue" :weight bold))
      (t (:background "gray30" :foreground "#009cff" :weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:background "#bb66ff"))))
   '(mode-line-inactive
     ((((class color) (min-colors 88))
       (:background "gray20" :foreground "gray70"
                    :box (:line-width -1 :color "gray40")))
      (((class color) (min-colors 16))
       (:background "gray20" :foreground "gray70"))
      (((class color) (min-colors 8))
       (:background "white" :foreground "black"))
      (((type tty) (class mono))
       (:background "white" :foreground "black"))
      (t (:background "gray20" :foreground "gray70"
                      :box (:line-width -1 :color "gray40")))))
   '(modeline-mousable ((t (:background "gray10" :foreground "white"))))
   '(modeline-mousable-minor-mode
     ((t (:background "gray10" :foreground "white")))))
  )


(eval-after-load "faces"
  '(basic-faces-settings))


(provide 'basic-faces-settings)