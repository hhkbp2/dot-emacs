;; -*- Emacs-Lisp -*-
;; Settings for `jde-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-04-08 21:27>

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
;; `jde-mode'
;; Java Development Enviroment

;;; Code:


;; load face settings
(require 'jde-face-settings)


(autoload 'jde-mode "jde" "JDE mode." t)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))


(defun jde-settings ()
  "Settings for `jde-mode'."

  (setq jde-enable-abbrev-mode t
        jde-complete-function 'jde-complete-menu
        ;; 注册所有安装的jdk版本
        jde-jdk-registry  '(("sun 1.6" . "/usr/lib/jvm/java-6-sun")
                            ("open 1.6" . "/usr/lib/jvm/java-6-openjdk/"))
        ;; 选择jde采用的jdk版本
        jde-jdk '("1.6")
        ;; 定义项目的类路径，会被传递到-classpath选项
        ;; 全局的类路径参考设置`jde-global-classpath'
        ;; 如没有前两者都没有设置，则使用环境变量CLASSPATH
        ;;  jde-compile-option-classpath '("./")
        jde-debugger '("JDEbug")
        jde-compiler '("javac" ""))
  )

(eval-after-load "jde-mode"
  `(jde-settings))


(provide 'jde-settings)
