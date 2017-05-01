;;; font-lock-face.el --- Settings for a few favorite font-lock faces

;;; Commentary:
;;
;; A few my favorite font-lock face
;;

;;; Code:


(defface dw-pair-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "skyblue"))
    (((class color) (min-colors 88) (background light)) (:foreground "skyblue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "blue"))
    (((class color) (min-colors 8) (background dark)) (:foreground "white"))
    (((class color) (min-colors 8) (background light)) (:foreground "black"))
    (((type tty) (class mono)) (:foreground "blue"))
    (t (:foreground "blue")))
  "Face to highlight parentheses.")


(defface dw-assign-operator-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "rosybrown"))
    (((class color) (min-colors 88) (background light)) (:foreground "rosybrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "white"))
    (((class color) (min-colors 16) (background light)) (:foreground "black"))
    (((class color) (min-colors 8) (background dark)) (:foreground "white"))
    (((class color) (min-colors 8) (background light)) (:foreground "black"))
    (((type tty) (class mono)) (:foreground "blue"))
    (t (:foreground "blue")))
  "Face to highlight assign operator \"=\".")


(defface dw-keywords-apue-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#bd2626"))
    (((class color) (min-colors 88) (background light)) (:foreground "#bd2626"))
    (((class color) (min-colors 16) (background dark)) (:foreground "darkred"))
    (((class color) (min-colors 16) (background light)) (:foreground "darkred"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face to highlight keywords of Advanced Programming in UNIX Environment.")


(defface dw-keywords-unp-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "royalblue"))
    (((class color) (min-colors 88) (background light)) (:foreground "royalblue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "royalblue"))
    (((class color) (min-colors 16) (background light)) (:foreground "royalblue"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face to highlight keywords of UNIX Network Programming.")


(defface dw-makefile-paren-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "magenta"))
    (((class color) (min-colors 88) (background light)) (:foreground "magenta"))
    (((class color) (min-colors 16) (background dark)) (:foreground "magenta"))
    (((class color) (min-colors 16) (background light)) (:foreground "magenta"))
    (((class color) (min-colors 8) (background dark)) (:foreground "white"))
    (((class color) (min-colors 8) (background light)) (:foreground "black"))
    (((type tty) (class mono)) (:foreground "red"))
    (t (:foreground "red")))
  "Face to highlight makefile parenthesesis \"$()\".")


(defface dw-keywords-cpp-namespace-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#ff6100"))
    (((class color) (min-colors 88) (background light)) (:foreground "#ff6100"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#ff6100"))
    (((class color) (min-colors 16) (background light)) (:foreground "#ff6100"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#ff6100")))
  "Face to highlight c++ namespace name.")


(defface dw-keywords-cpp-std-container-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library container (and adaptor) name.")


(defface dw-keywords-cpp-std-container-member-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library container (and adaptor) member name.")


(defface dw-keywords-cpp-std-container-typedef-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library container (and adaptor) typedef name.")


(defface dw-keywords-cpp-std-iterator-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library iterator name.")


(defface dw-keywords-cpp-std-algorithm-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library algorithm name.")


(defface dw-keywords-cpp-std-io-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library io class name.")


(defface dw-keywords-cpp-std-io-predef-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library io predefination name.")


(defface dw-keywords-cpp-std-functor-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library function object name.")


(defface dw-keywords-cpp-std-functor-adaptor-face
  '((((class color) (min-colors 88) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 88) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#fce94f"))
    (((class color) (min-colors 16) (background light)) (:foreground "#fce94f"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (((class color) (min-colors 8) (background light)) (:foreground "yellow"))
    (((type tty) (class mono)) (:foreground "yellow"))
    (t (:foreground "#fce94f")))
  "Face to highlight c++ standard library function object adaptor name.")


(provide 'font-lock-face)

;;; font-lock-face.el ends here
