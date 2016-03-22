;;; ibuffer-settings.el --- Settings for `ibuffer'
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:54>

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
;; `ibuffer'
;; a good buffer Manager

;;; Code:


;; load `ibuffer', if it isn't loaded yet
(require 'ibuffer)


(defadvice ibuffer (around ibuffer-goto-recent-buffer)
  "Goto the most recent buffer line after switch to ibuffer."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (or (equal recent-buffer-name "*Ibuffer*")
        (ibuffer-jump-to-buffer recent-buffer-name))))


(defun ibuffer-settings ()
  "Settings for `ibuffer'."

  ;; sort buffer by recent usage frequency
  (setq ibuffer-sorting-mode 'recency)
  ;; always open ibuffer with cursor pointing to the most recent buffer
  (setq ibuffer-always-show-last-buffer t)
  ;; display header line
  (setq ibuffer-use-header-line t)
  ;; do show the empty group
  (setq ibuffer-show-empty-filter-groups nil)

  ;; set format
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process)
          (mark " " (name 30 -1) " " filename)))

  ;; gnus-style grouping
  (setq ibuffer-saved-filter-groups
        (quote (("Default"
                 ("Dired" (mode . dired-mode))
                 ("ELisp" (or (mode . emacs-lisp-mode)))
                 ("Lisp" (or (mode . lisp-mode)
                             (mode . slime-repl-mode)))
                 ("Scheme" (or (mode . scheme-mode)
                               (mode . scheme-interaction-mode)))
                 ("Joxa" (mode . joxa-mode))
                 ("Elixir" (mode . elixir-mode))
                 ("Erlang" (mode . erlang-mode))
                 ("Clojure" (mode . clojure-mode))
                 ("Java" (mode . java-mode))
                 ("Ruby" (or (mode . ruby-mode)
                             (mode . enh-ruby-mode)))
                 ("Python" (mode . python-mode))
                 ("Rust" (mode . rust-mode))
                 ("Go" (mode . go-mode))
                 ("C/C++" (or (mode . c-mode)
                              (mode . c++-mode)))
                 ("Makefile" (or (mode . makefile-mode)
                                 (mode . makefile-automake-mode)
                                 (mode . makefile-gmake-mode)
                                 (mode . makefile-makepp-mode)
                                 (mode . makefile-bsdmake-mode)
                                 (mode . makeilfe-imake-mode)))
                 ("Thrift" (name . "\\.thrift$"))
                 ("Protobuf" (name . "\\.proto$"))
                 ("Shell" (or (mode . sh-mode)
                              (mode . shell-mode)
                              (mode . term-mode)))
                 ("Awk" (mode . awk-mode))
                 ("Sed" (name . "\\.sed$"))
                 ("Conf" (or (mode . conf-mode)
                             (mode . conf-unix-mode)
                             (mode . conf-colon-mode)
                             (mode . conf-space-mode)
                             (mode . conf-xdefaults-mode)))
                 ("Markdown" (mode . markdown-mode))
                 ("ReST" (mode . rst-mode))
                 ("Html" (or (mode . html-helper-mode)
                             (mode . html-mode)
                             (mode . css-mode)
                             (mode . sgml-mode)))
                 ("JavaScript" (mode . js-mode))
                 ("Toml" (mode . toml-mode))
                 ("Yaml" (mode . yaml-mode))
                 ("Json" (mode . json-mode))
                 ("Xml" (or (mode . nxml-mode)
                            (mode . xml-mode)
                            (mode . sgml-mode)))
                 ("SQL" (mode . sql-mode))
                 ("TeX" (or (mode . tex-mode)
                            (mode . latex-mode)))
                 ("Plan" (or (mode . calendar-mode)
                             ;;(name . "^\\*Calendar\\*$")
                             (mode . diary-mode)
                             ;;(name . "^diary$")
                             (mode . todoo-mode)
                             (mode . muse-mode)))
                 ("Yasnippet" (mode . snippet-mode))
                 ("Erc" (mode . erc-mode))
                 ("Gnus" (or (mode . message-mode)
                             (mode . bbdb-mode)
                             (mode . mail-mode)
                             (mode . gnus-group-mode)
                             (mode . gnus-summary-mode)
                             (mode . gnus-article-mode)
                             (name . "^\\.bbdb$")
                             (name . "^\\.newsrc-dribble")))
                 ("Text" (mode . text-mode))
                 ("Emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$")))
                 ("Help" (or (name . "\*Help\*")
                             (name . "\*Apropos\*")
                             (name . "\*info\*")))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              ;; ibuffer-auto-mode is a minor mode
              ;; that automatically keeps the buffer list up to date.
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "Default")))

  ;; activate advices
  (ad-activate 'ibuffer)
  )


(eval-after-load "ibuffer"
  `(ibuffer-settings))


(provide 'ibuffer-settings)

;;; ibuffer-settings.el ends here
