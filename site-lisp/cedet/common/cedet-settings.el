;; -*- Emacs-Lisp -*-

;; Time-stamp: <2012-09-15 16:04>

;; cedet1.0pre6 is conflict with which-func
;; after require cedet, which-func cann't work

;; http://emacser.com/install-cedet.htm
;; http://emacser.com/cedet.htm


(require 'cedet)
(require 'eieio-settings)
(require 'ede-settings)
(require 'cogre-settings)
(require 'semantic-settings)
(require 'srecode-settings)
(require 'speedbar-settings)
(require 'senator-face-settings)

;; 用pulse实现Emacs的淡入淡出效果
;; http://emacser.com/pulse.htm
(require 'pulse-settings)

;;;###autoload
(defun cedet-settings-4-info ()
  "`cedet' settings for `info'."
  (info-initialize)
  (dolist (package `("cogre" "common" "ede" "eieio" "semantic/doc" "speedbar" "srecode"))
    (add-to-list 'Info-directory-list (concat import-path "cedet/" package "/"))))

(eval-after-load "info"
  `(cedet-settings-4-info))


(provide 'cedet-settings)
