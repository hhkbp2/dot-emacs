;; basic-faces-settings.el --- Settings for basic faces
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:07>

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
   '(mouse ((t (nil))))
   '(cursor
     ((((type x)) (:background "#eeeeee"))
      (((class color) (min-colors 256)) (:background "#eeeeee"))
      (((class color) (min-colors 16)) (:background "blue"))
      (((class color) (min-colors 8)) (:background "blue"))
      (t (:background "blue"))))
   '(border ((t (:background "black"))))
   '(vertical-border ((t (nil))))
   '(escape-glyph ((t (:foreground "cyan"))))
   '(fixed-pitch ((t (:family "Monospace"))))
   '(variable-pitch ((t (:family "Sans Serif"))))
   '(fringe ((t (:background "#1A1A1A"))))
   '(header-line
     ((t (:background "#333333" :foreground "#E5E5E5" :weight semi-bold))))
   '(nobreak-space ((t (:foreground "cyan" :underline t))))
   '(shadow
     ((((type x)) (:foreground "#B3B3B3"))
      (((class color) (min-colors 256)) (:foreground "#B3B3B3"))
      (((class color) (min-colors 16)) (nil))
      (((class color) (min-colors 8)) (nil))
      (t (nil))))
   '(highlight
     ((((type x)) (:background "#222222"))
      (((class color) (min-colors 256)) (:background "#222222"))
      (((class color) (min-colors 16)) (:inverse-video t))
      (((class color) (min-colors 8)) (:inverse-video t))
      (t (:inverse-video t))))
   '(region
     ((((type x)) (:background "#555753"))
      (((class color) (min-colors 256)) (:background "#585858"))
      (((class color) (min-colors 16)) (:background "blue" :foreground "white"))
      (((class color) (min-colors 8)) (:background "blue" :foreground "white"))
      (t (:background "blue" :foreground "white"))))
   '(region-invert
     ((((type x)) (:background "#555753"))
      (((class color) (min-colors 256)) (:background "#585858"))
      (((class color) (min-colors 16)) (:background "blue" :foreground "white"))
      (((class color) (min-colors 8)) (:background "blue" :foreground "white"))
      (t (:background "blue" :foreground "white"))))
   '(secondary-selection ((t (:background "#bbbbbb" :foreground "#eeeeee"))))
   '(trailing-whitespace
     ((((type x)) (:background "white"))
      (((class color) (min-colors 256)) (:background "white"))
      (((class color) (min-colors 16)) (:background "white"))
      (((class color) (min-colors 8)) (:background "white"))
      (t (:background "white"))))
   '(link
     ((((type x))
       (:foreground "#5fafff" :underline t))
      (((class color) (min-colors 256))
       (:foreground "#5f87ff" :underline t))
      (((class color) (min-colors 16))
       (:foreground "#blue" :underline t))
      (((class color) (min-colors 8))
       (:foreground "blue" :underline t))
      (t (:foreground "blue" :underline t))))
   '(link-visited
     ((((type x))
       (:foreground "#af5faf" :underline t :slant italic))
      (((class color) (min-colors 256))
       (:foreground "#af5faf" :underline t :slant italic))
      (((class color) (min-colors 16))
       (:foreground "purple" :underline t :slant italic))
      (((class color) (min-colors 8))
       (:foreground "purple" :underline t :slant italic))
      (t (:foreground "purple" :underline t :slant italic))))
   '(minibuffer-prompt
     ((((type x))
       (:foreground "#fce94f" :weight semi-bold))
      (((class color) (min-colors 256))
       (:foreground "#ffdf5f" :weight semi-bold))
      (((class color) (min-colors 16))
       (:foreground "yellow" :weight bold))
      (((class color) (min-colors 8))
       (:foreground "yellow" :weight bold))
      (t (:foreground "yellow" :weight bold))))
   '(mode-line
     ((((type x))
       (:background "#1A1A1A" :foreground "#999999" :weight semi-bold
                    :box (:line-width -1 :color "#7F7F7F")))
      (((class color) (min-colors 256))
       (:background "#9e9e9e" :foreground "#303030" :weight semi-bold))
      (((class color) (min-colors 16))
       (:background "purple" :foreground "white" :weight semi-bold))
      (((class color) (min-colors 8))
       (:background "purple" :foreground "white" :weight semi-bold))
      (t (:background "purple" :foreground "white" :weight semi-bold))))
   '(mode-line-buffer-id
     ((((type x))
       (:background "#4D4D4D" :foreground "#009cff" :weight bold))
      (((class color) (min-colors 256))
       (:background "#585858" :foreground "#00afff" :weight bold))
      (((class color) (min-colors 16))
       (:background "black" :foreground "blue" :weight bold))
      (((class color) (min-colors 8))
       (:background "black" :foreground "blue" :weight bold))
      (t (:background "black" :foreground "blue" :weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:inverse-video t))))
   '(mode-line-inactive
     ((((type x))
       (:background "#333333" :foreground "#B3B3B3"
                    :box (:line-width -1 :color "#666666") :weight normal))
      (((class color) (min-colors 256))
       (:background "#808080" :foreground "#3a3a3a" :weight normal))
      (((class color) (min-colors 16))
       (:background "white" :foreground "black" :weight normal))
      (((class color) (min-colors 8))
       (:background "white" :foreground "black" :weight normal))
      (t (:background "white" :foreground "black" :weight normal))))
   '(modeline-mousable ((t (:background "#1A1A1A" :foreground "white"))))
   '(modeline-mousable-minor-mode
     ((t (:background "#1A1A1A" :foreground "white")))))
  )


(eval-after-load "faces"
  '(basic-faces-settings))


(provide 'basic-faces-settings)

;;; basic-faces-settings.el ends here
