;;; ido-settings.el --- Settings for `ido-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;
;; `ido-mode'
;; open file, find file and switch buffer
;;

;;; Code:


(require 'ido)
;; load face settings
(require 'ido-face-settings)
(require 'ido-complete-space-or-hyphen)
(require 'ido-ubiquitous)


(defun ido-settings ()
  "Settings for `ido-mode'."

  (setq ido-save-directory-list-file "~/.emacs.d/ido.last")
  ;; turn on ido mode
  (ido-mode 1)
  ;; toggle ido speed-ups everywhere file and directory names are read.
  (ido-everywhere 1)

  (ido-ubiquitous-mode 1)

  )

(eval-after-load "ido"
  `(ido-settings))


(provide 'ido-settings)

;;; ido-settings.el ends here
