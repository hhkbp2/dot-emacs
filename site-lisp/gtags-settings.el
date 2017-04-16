;;; gtags-settings.el --- Settings for `gtags-mode'
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(autoload 'gtags-mode "gtags")
(require 'dev-base-settings)
(require 'linum-settings)
(require 'dw-functionals)


(defadvice gtags-select-mode (after gtags-select-mode-linum-on)
  "Settings for `gtags-select-mode'."
  (linum-mode 1)
  ;; key-bindings
  (define-key gtags-mode-map [(meta \,)] 'gtags-pop-stack)
  (define-key gtags-mode-map [(meta .)] 'gtags-select-tag))


(defvar dw-office-repository-path-prefix-list
  '("/home" "/data" "/data1" "/data2")
  "The possible path prefix list of office machine repository.")


(defun dw-guess-gtags-root-dir ()
  "Return gtags root directory on current machine, a guess result."
  (if (dw-on-office-machine)
      (dw-get-real-dir dw-office-repository-path-prefix-list)
    (expand-directory-name "~/program")))


(defun gtags-settings ()
  "Settings for `gtags-mode'."

  (let ((dir (dw-guess-gtags-root-dir)))
    (if dir
        (setq gtags-rootdir dir)))

  ;; set code path display style in `gtags-select-mode'
  ;; alternative values: root, relative, absolute
  (setq gtags-path-style 'root)

  ;; activate advices
  (ad-activate 'gtags-select-mode)

  ;; key-bindings
  (define-key gtags-mode-map [(meta \,)] 'gtags-pop-stack)
  (define-key gtags-mode-map [(meta .)] 'gtags-find-tag-from-here))


(eval-after-load "gtags-mode"
  `(gtags-settings))


(dolist (mode-hook dev-mode-hook-list-static)
  (add-hook mode-hook
            '(lambda ()
               (gtags-mode 1)
               (gtags-settings))))


(provide 'gtags-settings)

;;; gtags-settings.el ends here
