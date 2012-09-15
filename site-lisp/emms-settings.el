;; -*- Emacs-Lisp -*-
;; Settings for `emms'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2011-02-05 15:20>

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
;; `emms'
;; Emacs MultiMedia System

;;; Code:


(require 'emms-setup)

(require 'emms-player-simple)
(require 'emms-player-mplayer)

(require 'emms-playlist-mode)

;; 自动保存和导入上次的播放列表
(require 'emms-history)

(require 'emms-mode-line)

;; 调整音量, no cli volume setup tools in windows
(require 'emms-volume)
;; 给音乐打分
(require 'emms-score)

;; 自动识别音乐标签的编码
(require 'emms-i18n)

;; load face settings
(require 'emms-face-settings)


(defun emms-settings ()
  "Settings for `emms'."

  (emms-standard)
  (emms-default-players)

  (emms-score 1)

  ;; Show the current track each time EMMS
  ;; starts to play a track with "NP : "
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-show-format "NP: %s")

  ;; 我的音乐路径
  (setq emms-source-file-default-directory "~/music")

  ;; 编码设置
  (setq emms-info-mp3info-coding-system 'gbk
        emms-cache-file-coding-system 'utf-8
        ;; emms-i18n-default-coding-system '(utf-8 . utf-8)
        )

  (setq emms-track-description-function
        'bigclean-emms-info-track-description)

  (setq emms-mode-line-mode-line-function
        'bigclean-emms-mode-line-playlist-current)

  ;; key-bindings
  (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
  (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
  (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
  (define-key emms-playlist-mode-map (kbd "<right>") (lambda ()
                                                       (interactive)
                                                       (emms-seek +10)))
  (define-key emms-playlist-mode-map (kbd "<left>") (lambda ()
                                                      (interactive)
                                                      (emms-seek -10)))
  (define-key emms-playlist-mode-map (kbd "<up>") (lambda ()
                                                    (interactive)
                                                    (emms-seek +60)))
  (define-key emms-playlist-mode-map (kbd "<down>") (lambda ()
                                                      (interactive)
                                                      (emms-seek -60)))
  )


;; customize playlist format
(defun bigclean-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title))
        (album (emms-track-get track 'info-album))
        (ptime (emms-track-get track 'info-playing-time)))
    (if title
        (format
         "%-35s %-40s %-35s %5s:%-5s"
         (if artist artist "")
         (if title title "")
         (if album album "")
         (/ ptime 60)
         (% ptime 60)))))


;; format current track,only display title in mode line
(defun bigclean-emms-mode-line-playlist-current ()
  "Return a description of the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and title)
                   title)))))


(eval-after-load "emms"
  `(emms-settings))


(provide 'emms-settings)
