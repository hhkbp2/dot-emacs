;; color-theme-darkmate.el --- The darkmate color theme for emacs
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2009, 2010, 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2017-04-16 20:16>

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
;; The darkmate color theme for emacs is a color scheme samiliar with the
;; darkmate color theme of gEdit. It depends on the color-theme.el
;; which is in the color-theme package for Emacs.

;;; Installation:
;;
;; Add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "path/color-theme-darkmate.el")
;;
;; and then evaluate (color-theme-darkmate) to activate it.

;;; Code:


(eval-when-compile (require 'color-theme))


(defun color-theme-darkmate ()
  "A color theme inspired by the darkmate color theme of gEdit,
which is inspired by the default color theme of TextMate.
Created by Dylan.Wen <hhkbp2@gmail.com>, 2009-02-09."
  (interactive)
  ;;
  ;; Brief colors used in this theme:
  ;; white
  ;; (color-white "#eeeeee")
  ;; gray -> dark
  ;; (color-gray "#bbbbbb")
  ;; (color-asfalto "#555753")
  ;; (color-carbon "#232323")
  ;; green -> deep
  ;; (color-senape "#acc900")
  ;; (color-lime "#96ff00")
  ;; (color-green "#00c900")
  ;; green -> blue
  ;; (color-alga "#00c99b")
  ;; (color-aque "#00d8ff")
  ;; (color-cyan "#009cff")
  ;; purple
  ;; (color-violet "#9e91ff")
  ;; (color-purple "#bb66ff")
  ;; pink -> deep red
  ;; (color-magenta "#ff79d9")
  ;; (color-fuschsia "#ff44cc")
  ;; (color-red "#ff2f6a")
  ;; yellow
  ;; (color-yellow "#fce94f")
  ;; orange -> deep
  ;; (color-ambra "#ff9900")
  ;; (color-orange "#ff6100")
  ;;
  (color-theme-install
   '(color-theme-darkmate
     ((background-color . "#232323")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#eeeeee")
      (foreground-color . "#eeeeee")
      (mouse-color . "sienna1"))
     ((ac-fuzzy-cursor-color . "red")
      (ibuffer-deletion-face . font-lock-warning-face)
      (ibuffer-filter-group-name-face . bold)
      (ibuffer-marked-face . font-lock-type-face)
      (ibuffer-title-face . font-lock-comment-face)
      (ispell-highlight-face . flyspell-incorrect)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (term-default-bg-color . "#232323")
      (term-default-fg-color . "#eeeeee")
      (todoo-item-assigned-header-face . todoo-item-assigned-header-face)
      (todoo-item-header-face . todoo-item-header-face)
      (todoo-sub-item-header-face . todoo-sub-item-header-face)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default
       ((t (:background "#232323" :foreground "#eeeeee"
                        :family "Vera Sans YuanTi Mono"
                        :height 115 :width normal :weight normal :slant normal
                        :box nil  :inverse-video nil :strike-through nil
                        :overline nil :underline nil :stipple nil
                        :foundry "unknown"))))
     (Term-default-fg-inv ((t (nil))))
     (ac-candidate-face
      ((((class color) (min-colors 88))
        (:background "lightgray" :foreground "black"))
       (((class color) (min-colors 16))
        (:background "lightgray" :foreground "black"))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"))
       (((type tty) (class mono))
        (:background "white" :foreground "black"))
       (t (:background "lightgray" :foreground "black"))))
     (ac-completion-face
      ((((class color) (min-colors 88))
        (:background "darkgray" :underline t))
       (((class color) (min-colors 16))
        (:background "darkgray" :underline t))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"))
       (((type tty) (class mono))
        (:background "white" :foreground "black"))
       (t (:foreground "darkgray" :underline t))))
     (ac-gtags-candidate-face
      ((((class color) (min-colors 88))
        (:background "lightgray" :foreground "navy"))
       (((class color) (min-colors 16))
        (:background "lightgray" :foreground "navy"))
       (((class color) (min-colors 8))
        (:background "white" :foreground "green"))
       (((type tty) (class mono))
        (:background "white" :foreground "green"))
       (t (:background "lightgray" :foreground "navy"))))
     (ac-gtags-selection-face
      ((((class color) (min-colors 88))
        (:background "navy" :foreground "white" :weight bold))
       (((class color) (min-colors 16))
        (:background "navy" :foreground "white" :weight bold))
       (((class color) (min-colors 8))
        (:background "green" :foreground "white" :weight bold))
       (((type tty) (class mono))
        (:background "green" :foreground "white" :weight bold))
       (t (:background "navy" :foreground "white" :weight bold))))
     (ac-selection-face
      ((((class color) (min-colors 88))
        (:background "steelblue" :foreground "white" :weight bold))
       (((class color) (min-colors 16))
        (:background "steelblue" :foreground "white" :weight bold))
       (((class color) (min-colors 8))
        (:background "cyan" :foreground "white" :weight bold))
       (((type tty) (class mono))
        (:background "cyan" :foreground "white" :weight bold))
       (t (:background "steelblue" :foreground "white" :weight bold))))
     (ac-yasnippet-candidate-face
      ((((class color) (min-colors 88))
        (:background "sandybrown" :foreground "black"))
       (((class color) (min-colors 16))
        (:background "sandybrown" :foreground "black"))
       (((class color) (min-colors 8))
        (:background "yellow" :foreground "black"))
       (((type tty) (class mono))
        (:background "yellow" :foreground "black"))
       (t (:background "sandybrown" :foreground "black"))))
     (ac-yasnippet-selection-face
      ((((class color) (min-colors 88))
        (:background "coral3" :foreground "white" :weight bold))
       (((class color) (min-colors 16))
        (:background "coral3" :foreground "white" :weight bold))
       (((class color) (min-colors 8))
        (:background "purple" :foreground "white" :weight bold))
       (((type tty) (class mono))
        (:background "purple" :foreground "white" :weight bold))
       (t (:background "coral3" :foreground "white" :weight bold))))
     (bg:erc-color-face0 ((t (:background "White"))))
     (bg:erc-color-face1 ((t (:background "black"))))
     (bg:erc-color-face10 ((t (:background "lightblue1"))))
     (bg:erc-color-face11 ((t (:background "cyan"))))
     (bg:erc-color-face12 ((t (:background "blue"))))
     (bg:erc-color-face13 ((t (:background "deeppink"))))
     (bg:erc-color-face14 ((t (:background "gray50"))))
     (bg:erc-color-face15 ((t (:background "gray90"))))
     (bg:erc-color-face2 ((t (:background "blue4"))))
     (bg:erc-color-face3 ((t (:background "green4"))))
     (bg:erc-color-face4 ((t (:background "red"))))
     (bg:erc-color-face5 ((t (:background "brown"))))
     (bg:erc-color-face6 ((t (:background "purple"))))
     (bg:erc-color-face7 ((t (:background "orange"))))
     (bg:erc-color-face8 ((t (:background "yellow"))))
     (bg:erc-color-face9 ((t (:background "green"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:slant italic :weight bold))))
     (border-glyph ((t (nil))))
     (buffer-menu-buffer ((t (:weight bold))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (button ((t (:underline t))))
     (c-nonbreakable-space-face ((t (:background "#ff2f6a" :weight bold))))
     (comint-highlight-input ((t (:weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan1"))))
     (completions-annotations ((t (:slant italic))))
     (completions-common-part ((t (:inherit default))))
     (completions-first-difference ((t (:weight bold))))
     (dired-directory
      ((((class color) (min-colors 88))
        (:foreground "#9e91ff" :weight bold))
       (((class color) (min-colors 16))
        (:foreground "#9e91ff" :weight bold))
       (((class color) (min-colors 8))
        (:foreground "blue" :weight bold))
       (((type tty) (class mono))
        (:foreground "blue" :weight bold))
       (t (:foreground "#9e91ff"))))
     (dired-flagged ((t (:weight bold :foreground "Pink"))))
     (dired-header
      ((((class color) (min-colors 88))
        (:foreground "#bb66ff" :slant italic :weight bold))
       (((class color) (min-colors 16))
        (:foreground "purple" :slant italic :weight bold))
       (((class color) (min-colors 8))
        (:foreground "purple" :slant italic :weight bold))
       (((type tty) (class mono))
        (:foreground "purple" :slant italic :weight bold))
       (t (:foreground "#bb66ff" :weight bold))))
     (dired-ignored
      ((((class color) (min-colors 88)) (:foreground "grey70"))
       (((class color) (min-colors 16)) (:foreground "grey70"))
       (((class color) (min-colors 8)) (:foreground "white"))
       (((type tty) (class mono)) (:foreground "white"))
       (t (:foreground "grey70"))))
     (dired-mark
      ((((class color) (min-colors 88)) (:foreground "#009cff"))
       (((class color) (min-colors 16)) (:foreground "cyan"))
       (((class color) (min-colors 8)) (:foreground "cyan"))
       (((type tty) (class mono)) (:foreground "cyan"))
       (t (:foreground "#009cff"))))
     (dired-marked
      ((((class color) (min-colors 88)) (:foreground "#009cff" :weight bold))
       (((class color) (min-colors 16)) (:foreground "cyan" :weight bold))
       (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
       (((type tty) (class mono)) (:foreground "cyan" :weight bold))
       (t (:foreground "#009cff" :weight bold))))
     (dired-perm-write
      ((((class color) (min-colors 88)) (:foreground "#00c900" :slant italic))
       (((class color) (min-colors 16)) (:foreground "#00c900" :slant italic))
       (((class color) (min-colors 8)) (:foreground "green" :slant italic))
       (((type tty) (class mono)) (:foreground "green" :slant italic))
       (t (:foreground "#00c900" :slant italic))))
     (dired-symlink
      ((((class color) (min-colors 88)) (:foreground "#bb66ff"))
       (((class color) (min-colors 16)) (:foreground "#bb66ff"))
       (((class color) (min-colors 8)) (:foreground "purple"))
       (((type tty) (class mono)) (:foreground "purple"))
       (t (:foreground "#bb66ff"))))
     (dired-warning
      ((((class color) (min-colors 88)) (:foreground "#ff2f6a" :weight bold))
       (((class color) (min-colors 16)) (:foreground "red" :weight bold))
       (((class color) (min-colors 8)) (:foreground "red" :weight bold))
       (((type tty) (class mono)) (:foreground "red" :weight bold))
       (t (:foreground "#ff2f6a" :weight bold))))
     (dropdown-list-face
      ((t (:inherit default :background "lightyellow" :foreground "red"))))
     (dropdown-list-selection-face
      ((t (:inherit default :foreground "black" :background "purple"))))
     (eldoc-highlight-function-argument ((t (:weight bold))))
     (fg:erc-color-face0 ((t (:foreground "White"))))
     (fg:erc-color-face1 ((t (:foreground "black"))))
     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))
     (fg:erc-color-face11 ((t (:foreground "cyan"))))
     (fg:erc-color-face12 ((t (:foreground "blue"))))
     (fg:erc-color-face13 ((t (:foreground "deeppink"))))
     (fg:erc-color-face14 ((t (:foreground "gray50"))))
     (fg:erc-color-face15 ((t (:foreground "gray90"))))
     (fg:erc-color-face2 ((t (:foreground "blue4"))))
     (fg:erc-color-face3 ((t (:foreground "green4"))))
     (fg:erc-color-face4 ((t (:foreground "red"))))
     (fg:erc-color-face5 ((t (:foreground "brown"))))
     (fg:erc-color-face6 ((t (:foreground "purple"))))
     (fg:erc-color-face7 ((t (:foreground "orange"))))
     (fg:erc-color-face8 ((t (:foreground "yellow"))))
     (fg:erc-color-face9 ((t (:foreground "green"))))
     (file-name-shadow ((t (:foreground "grey70"))))
     (font-lock-builtin-face
      ((((class color) (min-colors 88)) (:foreground "#fce94f"))
       (((class color) (min-colors 16)) (:foreground "yellow"))
       (((class color) (min-colors 8)) (:foreground "yellow"))
       (((type tty) (class mono)) (:foreground "yellow"))
       (t (:foreground "#fce94f"))))
     (font-lock-comment-delimiter-face
      ((((class color) (min-colors 88)) (:foreground "#bb66ff" :slant italic))
       (((class color) (min-colors 16)) (:foreground "purple" :slant italic))
       (((class color) (min-colors 8)) (:foreground "purple" :slant italic))
       (((type tty) (class mono)) (:foreground "purple" :slant italic))
       ((t (:foreground "#bb66ff" :slant italic)))))
     (font-lock-comment-face
      ((((class color) (min-colors 88))
        (:foreground "#bb66ff" :slant italic :weight bold))
       (((class color) (min-colors 16))
        (:foreground "purple" :slant italic :weight bold))
       (((class color) (min-colors 8))
        (:foreground "purple" :slant italic :weight bold))
       (((type tty) (class mono))
        (:foreground "purple" :slant italic :weight bold))
       (t (:foreground "#bb66ff" :slant italic :weight bold))))
     (font-lock-constant-face
      ((((class color) (min-colors 88)) (:foreground "#ff44cc"))
       (((class color) (min-colors 16)) (:foreground "#ff44cc"))
       (((class color) (min-colors 8)) (:foreground "red"))
       (((type tty) (class mono)) (:foreground "red"))
       (t (:foreground "#ff44cc"))))
     (font-lock-doc-face
      ((((class color) (min-colors 88)) (:foreground "#96ff00"))
       (((class color) (min-colors 16)) (:foreground "limegreen"))
       (((class color) (min-colors 8)) (:foreground "green"))
       (((type tty) (class mono)) (:foreground "green"))
       (t (:foreground "#96ff00"))))
     (font-lock-doc-string-face
      ((((class color) (min-colors 88)) (:foreground "DarkOrange"))
       (((class color) (min-colors 16)) (:foreground "orange"))
       (((class color) (min-colors 8)) (:foreground "yellow"))
       (((type tty) (class mono)) (:foreground "yellow"))
       (t (:foreground "DarkOrange"))))
     (font-lock-function-name-face
      ((((class color) (min-colors 88)) (:foreground "#9e91ff" :weight book))
       (((class color) (min-colors 16)) (:foreground "blueviolet" :weight book))
       (((class color) (min-colors 8)) (:foreground "blue" :weight book))
       (((type tty) (class mono)) (:foreground "blue" :weight book))
       (t (:foreground "#9e91ff" :weight book))))
     (font-lock-keyword-face
      ((((class color) (min-colors 88)) (:foreground "#ff9900" :weight bold))
       (((class color) (min-colors 16)) (:foreground "orange" :weight bold))
       (((class color) (min-colors 8)) (:foreground "yellow" :weight bold))
       (((type tty) (class mono)) (:foreground "yellow" :weight bold))
       (t (:foreground "#ff9900" :weight bold))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face
      ((((class color) (min-colors 88)) (:foreground "#00d8ff" :weight bold))
       (((class color) (min-colors 16)) (:foreground "aquamarine" :weight bold))
       (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
       (((type tty) (class mono)) (:foreground "cyan" :weight bold))
       (t (:foreground "#00d8ff" :weight bold))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-regexp-grouping-backslash
      ((((class color) (min-colors 88)) (:foreground "#bbbbbb"))
       (((class color) (min-colors 16)) (:foreground "red"))
       (((class color) (min-colors 8)) (:foreground "red"))
       (((type tty) (class mono)) (:foreground "red"))
       (t (:foreground "#bbbbbb"))))
     (font-lock-regexp-grouping-construct
      ((((class color) (min-colors 88)) (:foreground "#ff44cc"))
       (((class color) (min-colors 16)) (:foreground "red"))
       (((class color) (min-colors 8)) (:foreground "red"))
       (((type tty) (class mono)) (:foreground "red"))
       (t (:foreground "#ff44cc"))))
     (font-lock-string-face
      ((((class color) (min-colors 88)) (:foreground "#96ff00"))
       (((class color) (min-colors 16)) (:foreground "limegreen"))
       (((class color) (min-colors 8)) (:foreground "green"))
       (((type tty) (class mono)) (:foreground "green"))
       (t (:foreground "#96ff00"))))
     (font-lock-type-face
      ((((class color) (min-colors 88)) (:foreground "#009cff"))
       (((class color) (min-colors 16)) (:foreground "cyan"))
       (((class color) (min-colors 8)) (:foreground "cyan"))
       (((type tty) (class mono)) (:foreground "cyan"))
       (t (:foreground "#009cff"))))
     (font-lock-variable-name-face
      ((((class color) (min-colors 88)) (:foreground "#00c900"))
       (((class color) (min-colors 16)) (:foreground "#00c900"))
       (((class color) (min-colors 8)) (:foreground "green"))
       (((type tty) (class mono)) (:foreground "green"))
       (t (:foreground "#00c900"))))
     (font-lock-warning-face
      ((((class color) (min-colors 88)) (:foreground "#ff2f6a" :weight bold))
       (((class color) (min-colors 16)) (:foreground "red" :weight bold))
       (((class color) (min-colors 8)) (:foreground "red" :weight bold))
       (((type tty) (class mono)) (:foreground "red" :weight bold))
       (t (:foreground "#ff2f6a" :weight bold))))
     (help-argument-name
      ((t (:blod t :foreground "green" :slant italic :weight bold))))
     (hi-black-b ((t (:weight bold))))
     (hi-black-hb ((t (:family "Sans Serif" :weight bold :height 1.67))))
     (hi-blue ((t (:background "light blue" :foreground "black"))))
     (hi-blue-b ((t (:foreground "blue1" :weight bold))))
     (hi-green ((t (:background "green1" :foreground "black"))))
     (hi-green-b ((t (:foreground "green1" :weight bold))))
     (hi-pink ((t (:background "pink" :foreground "black"))))
     (hi-red-b ((t (:foreground "red" :weight bold))))
     (hi-yellow ((t (:background "yellow1" :foreground "black"))))
     (hide-ifdef-shadow
      ((((class color) (min-colors 88)) (:foreground "grey70"))
       (((class color) (min-colors 16)) (:foreground "grey70"))
       (((class color) (min-colors 8)) (nil))
       (((type tty) (class mono)) (nil))
       (t (:foreground "grey70"))))
     (highlight-current-line-face
      ((((class color) (min-colors 88)) (:background "black"))
       (((class color) (min-colors 16)) (:background "black"))
       (((class color) (min-colors 8)) (nil))
       (((type tty) (class mono)) (nil))
       (t (:background "black"))))
     (highlight-symbol-face
      ((((class color) (min-colors 88))
        (:background "gray30"))
       (((class color) (min-colors 16))
        (:background "gray30"))
       (((class color) (min-colors 8))
        (:background "purple" :foreground "black"))
       (((type tty) (class mono))
        (:background "purple" :foreground "black"))
       (t (:background "gray30"))))
     (highline-face
      ((((class color) (min-colors 88))
        (:background "SeaGreen"))
       (((class color) (min-colors 16))
        (:background "SeaGreen"))
       (((class color) (min-colors 8))
        (:background "cyan" :foreground "black"))
       (((type tty) (class mono))
        (:background "cyan" :foreground "black"))
       (t (:background "SeaGreen"))))
     (italic ((t (:slant italic))))
     (left-margin ((t (nil))))
     (match
      ((((class color) (min-colors 88))
        (:background "RoyalBlue3" :foreground "#232323" :weight bold))
       (((class color) (min-colors 16))
        (:background "RoyalBlue3" :foreground "#232323" :weight bold))
       (((class color) (min-colors 8))
        (:background "blue" :foreground "black"))
       (((type tty) (class mono))
        (:background "blue" :foreground "black"))
       (t (:background "RoyalBlue3" :foreground "#232323" :weight bold))))
     (next-error ((t (:background "#555753"))))
     (outline-1 ((t (:foreground "#9e91ff"))))
     (outline-2 ((t (:foreground "#FF6400"))))
     (outline-3 ((t (:foreground "#ff9900" :weight bold))))
     (outline-4 ((t (:foreground "#bb66ff" :slant italic))))
     (outline-5 ((t (:foreground "#8DA6CE"))))
     (outline-6 ((t (:foreground "#fce94f"))))
     (outline-7 ((t (:foreground "cyan"))))
     (outline-8 ((t (:foreground "#96ff00"))))
     (popup-face ((t (:background "lightgray" :foreground "black"))))
     (popup-isearch-match ((t (:background "sky blue"))))
     (popup-menu-face ((t (:background "lightgray" :foreground "black"))))
     (popup-menu-selection-face
      ((t (:background "steelblue" :foreground "white"))))
     (popup-scroll-bar-background-face ((t (:background "gray"))))
     (popup-scroll-bar-foreground-face ((t (:background "black"))))
     (popup-tip-face ((t (:background "khaki1" :foreground "black"))))
     (pp^L-highlight ((t (:box (:line-width 3 :style pressed-button)))))
     (pulse-highlight-face ((t (:background "#AAAA33"))))
     (pulse-highlight-start-face ((t (:background "#AAAA33"))))
     (query-replace
      ((((class color) (min-colors 88))
        (:background "palevioletred2" :foreground "brown4"))
       (((class color) (min-colors 16))
        (:background "palevioletred2" :foreground "brown4"))
       (((class color) (min-colors 8))
        (:background "red" :foreground "black"))
       (((type tty) (class mono))
        (:background "red" :foreground "black"))
       (t (:background "palevioletred2" :foreground "brown4"))))
     (setnu-line-number-face
      ((t (:background "Grey15" :foreground "#555753" :weight bold))))
     (template-message-face ((t (:weight bold))))
     (term-black ((t (:foreground "black"))))
     (term-blackbg ((t (nil))))
     (term-blue ((t (:foreground "blue"))))
     (term-blue-bold-face ((t (:foreground "blue" :weight bold))))
     (term-blue-face ((t (:foreground "blue"))))
     (term-blue-inv-face ((t (:background "blue"))))
     (term-blue-ul-face ((t (:foreground "blue" :underline t))))
     (term-bluebg ((t (:background "blue"))))
     (term-bold ((t (:weight bold))))
     (term-cyan ((t (:foreground "cyan"))))
     (term-cyan-bold-face ((t (:foreground "cyan" :weight bold))))
     (term-cyan-face ((t (:foreground "cyan"))))
     (term-cyan-inv-face ((t (:background "cyan"))))
     (term-cyan-ul-face ((t (:foreground "cyan" :underline t))))
     (term-cyanbg ((t (:background "cyan"))))
     (term-default-bg ((t (:background "#232323"))))
     (term-default-bg-inv ((t (nil))))
     (term-default-bold-face ((t (:weight bold))))
     (term-default-face ((t (nil))))
     (term-default-fg ((t (:foreground "#eeeeee"))))
     (term-default-inv-face ((t (:background "peachpuff" :foreground "black"))))
     (term-default-ul-face ((t (:underline t))))
     (term-green ((t (:foreground "green"))))
     (term-green-bold-face ((t (:foreground "green" :weight bold))))
     (term-green-face ((t (:foreground "green"))))
     (term-green-inv-face ((t (:background "green"))))
     (term-green-ul-face ((t (:foreground "green" :underline t))))
     (term-greenbg ((t (:background "green"))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-magenta ((t (:foreground "magenta"))))
     (term-magenta-bold-face ((t (:foreground "magenta" :weight bold))))
     (term-magenta-face ((t (:foreground "magenta"))))
     (term-magenta-inv-face ((t (:background "magenta"))))
     (term-magenta-ul-face ((t (:foreground "magenta" :underline t))))
     (term-magentabg ((t (:background "magenta"))))
     (term-red ((t (:foreground "red"))))
     (term-red-bold-face ((t (:foreground "red" :weight bold))))
     (term-red-face ((t (:foreground "red"))))
     (term-red-inv-face ((t (:background "red"))))
     (term-red-ul-face ((t (:foreground "red" :underline t))))
     (term-redbg ((t (:background "red"))))
     (term-underline ((t (:underline t))))
     (term-white ((t (:foreground "white"))))
     (term-white-bold-face ((t (:foreground "white" :weight bold))))
     (term-white-face ((t (:foreground "white"))))
     (term-white-inv-face ((t (nil))))
     (term-white-ul-face ((t (:foreground "white" :underline t))))
     (term-whitebg ((t (:background "white"))))
     (term-yellow ((t (:foreground "yellow"))))
     (term-yellow-bold-face ((t (:foreground "yellow" :weight bold))))
     (term-yellow-face ((t (:foreground "yellow"))))
     (term-yellow-inv-face ((t (:background "yellow"))))
     (term-yellow-ul-face ((t (:foreground "yellow" :underline t))))
     (term-yellowbg ((t (:background "yellow"))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (todoo-item-assigned-header-face ((t (:foreground "red" :weight bold))))
     (todoo-item-header-face ((t (:foreground "goldenrod" :weight bold))))
     (todoo-sub-item-header-face ((t (:foreground "darkgoldenrod"))))
     (tooltip
      ((t
        (:background "lightyellow" :foreground "black" :family "Sans Serif"))))
     (underline ((t (:underline t))))
     (which-func
      ((((class color) (min-colors 88))
        (:background "#232323" :foreground "#9e91ff" :weight book))
       (((class color) (min-colors 16))
        (:background "#232323" :foreground "#9e91ff" :weight book))
       (((class color) (min-colors 8))
        (:background "white" :foreground "blue" :weight book))
       (((type tty) (class mono))
        (:background "white" :foreground "blue" :weight book))
       (t (:background "#232323" :foreground "#9e91ff" :weight book))))
     (widget-button ((t (:weight bold))))
     (widget-button-pressed ((t (:foreground "red"))))
     (widget-documentation ((t (:foreground "limegreen"))))
     (widget-field ((t (:background "dim gray"))))
     (widget-inactive ((t (:foreground "grey70"))))
     (widget-single-line-field ((t (:background "dim gray"))))
     (yas/field-debug-face ((t (nil))))
     (yas/field-highlight-face
      ((((class color) (min-colors 88))
        (:background "DimGrey"))
       (((class color) (min-colors 16))
        (:background "DimGrey"))
       (((class color) (min-colors 8))
        (:background "white" :foreground "black"))
       (((type tty) (class mono))
        (:background "white" :foreground "black"))
       (t (:background "DimGrey"))))
     (zmacs-region ((t (:background "snow" :foreground "blue")))))))


(add-to-list 'color-themes
             '(color-theme-darkmate
               "Darkmate"
               "Dylan.Wen <hhkbp2@gmail.com>"))


(provide 'color-theme-darkmate)

;;; color-theme-darkmate.el ends here
