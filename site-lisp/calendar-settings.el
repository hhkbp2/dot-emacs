;;; calendar-settings.el --- Settings for `calendar'
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


(require 'cal-china-x-settings)
(require 'calendar-face-settings)
(require 'dw-functionals)


(defun calendar-settings ()
  "Settings for `calendar'."

  ;; 设置我所在地方的经纬度，日月食的预测功能，和经纬度相联系的
  (setq calendar-latitude +22.55)
  (setq calendar-longitude +114.10)

  ;; 我的所在地
  (setq calendar-location-name "广州")

  ;; delete the frame no longer needed
  (setq calendar-remove-frame-by-deleting t)

  ;; 设定一周的开始为周一
  (setq calendar-week-start-day 1)

  ;; 节日和生日提醒设置
  ;; 全年公历,农历节日等重要日子
  (setq dw-holidays
        '(;; 格式:
          ;; 日历类型(公历，农历) 月 日 描述
          ;;
          ;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 3 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)))

  (dw-load-related-file-if-exist "../personal/dw-calendar-settings.el")

  ;; 日历不和日记相连，我不用Calendar自带的diary记日记
  (setq mark-diary-entries-in-calendar nil)

  ;; 在日历中突出标记节日和生日
  (setq mark-holidays-in-calendar t)
  ;; 打开calendar时自动打开节日和生日列表
  ;;(setq view-calendar-holidays-initially t)

  ;; 只查看我自己定制的节日
  (setq calendar-holidays dw-holidays))


(eval-after-load "calendar"
  `(calendar-settings))


(provide 'calendar-settings)

;;; calendar-settings.el ends here
