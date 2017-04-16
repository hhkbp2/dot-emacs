;;; dict-settings.el --- Settings for dictionary packages
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(require 'youdao-dictionary)
(require 'popwin)


(defun youdao-dictionary-settings ()
  "Settings for `youdao-dictionary'."

  ;; Enable Cache
  (setq url-automatic-caching t)

  ;; Example Key binding
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (push "*Youdao Dictionary*" popwin:special-display-config)

  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file "~/.emacs.d/youdao")

  ;; Enable Chinese word segmentation support (支持中文分词)
  ;; (setq youdao-dictionary-use-chinese-word-segmentation t)
  )


(eval-after-load "youdao-dictionary"
  `(youdao-dictionary-settings))


;; load `osx-dictionary' on Mac OS
(when (eq system-type `darwin)
  (require 'osx-dictionary)

  (defun osx-dictionary-settings ()
    "Settings for `osx-dictionary'."

    ;; Choose explicitly a dictionary for searching (use the first available
    ;; dictionary in Dictionary.app if not set)
    ;; (setq osx-dictionary-dictionary-choice "Apple")
    ;; To search in more than one dictionaries
    ;; (setq osx-dictionary-dictionary-choice (list "English" "Simplified Chinese" "Spanish"))
    ;; To search in all dictionaries
    ;; (setq osx-dictionary-dictionary-choice (osx-dictionary-get-all-dictionaries))

    ;; Key bindings
    ;;(global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
    ;; (global-set-key (kbd "C-c i") 'osx-dictionary-search-input)

    ;; Work with popwin-el (https://github.com/m2ym/popwin-el)
    (push "*osx-dictionary*" popwin:special-display-config)

    ;; Support Chinese word
    ;; (setq osx-dictionary-use-chinese-text-segmentation t)
    )

  (eval-after-load "osx-dictionary"
    `(osx-dictionary-settings))

  )


(provide 'dict-settings)

;;; dict-settings.el ends here
