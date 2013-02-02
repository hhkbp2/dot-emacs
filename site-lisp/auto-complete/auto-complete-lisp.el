;; auto-complete-lisp.el --- Settings for `auto-complete' in lisp modes.
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2012 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2012-01-23 14:33>

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


(require 'auto-complete)
(require 'auto-complete-config)
(require 'util)


(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-omni-completion-sources
        '(("\\<featurep\s+'" ac+-source-elisp-features)
          ("\\<require\s+'"  ac+-source-elisp-features)
          ("\\<load\s+\""    ac-source-emacs-lisp-features)))
  (ac+-apply-source-elisp-faces)
  (setq ac-sources
        '(ac-source-features
          ac-source-functions
          ac-source-yasnippet
          ac-source-variables
          ac-source-symbols
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-words-in-same-mode-buffers)))


(defun auto-complete-lisp ()
  ;; setup auto-complete
  (ac-settings-4-lisp))


(am-add-hooks
 `(emacs-lisp-mode-hook scheme-mode-hook lisp-mode-hook
                        lisp-interaction-mode-hook)
 'auto-complete-lisp)


(provide 'auto-complete-lisp)

;;; auto-complete-lisp.el ends here
