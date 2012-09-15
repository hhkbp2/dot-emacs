;; -*- Emacs-Lisp -*-
;; Settings for spell checking.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Time-stamp: <2012-07-15 16:51>

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


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(require 'dev-base)
(require 'flyspell-face-settings)


(defun spell-checking-settings ()
  "Settings for spell checking."

  ;; use apsell as ispell backend
  (setq-default ispell-program-name "aspell")
  ;; use American English as ispell default dictionary
  (ispell-change-dictionary "american" t)

  ;; (global-flyspell-mode)

  (dolist (hook
           `(;;text-mode-hook
             ;;,@dev-mode-hook-list
             ))
    (add-hook hook 'flyspell-mode))
  )

(spell-checking-settings)
;;(eval-after-load "flyspell"
;;  `(spell-checking-settings))


(provide 'spell-checking-settings)