;; wlc.el --- wonderful lisp coding extension
;; -*- Emacs-Lisp -*-
;; A few features to make wonderful lisp coding exprience in emacs

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-02-18 23:19>

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
;; Wonderful Lisp Coding extension is an Emacs extension aimed to provide
;; Wonderful Lisp Coding exprience and to make
;; Writing Lisp Comfortable in Emacs.
;;
;; It contains a few features:
;;
;; 1. hungry delete for Lisp
;;   A port from hungry delete in cc-mode.
;;   To enable it, first load this file:
;;       (require 'wlc)
;;   and then turn on hungry delete feature using command:
;;       (wlc/toggle-hungry-state 1)
;;   It is fairly similar to hungry delete in cc-mode.
;;   Notice: make sure `wlc/fast-delete-flag' is turn on
;;   when you feel delete or backspace is slow.
;;
;; 2. electric indentation for Lisp
;;   A port from electric indentation in cc-mode.
;;   Load `wlc' and it is enabled by default.
;;   Use `wlc/toggle-electric-state' to turn it on or off.
;;   This electric indentation feature is much simpler than what it is
;;   in cc-mode:
;;   Unlike c & c++, which have lots of different language syntax components,
;;   Lisp has a united form of program text organization, that is,
;;   everything is list and put in parentheses.  The "smart" indentation
;;   for Lisp currently has only:
;;     1) indent on inserting newline
;;        no more key is required to press but `enter' (or in another name,
;;        the `return' key).
;;     2) try to indent things as much as possible when it's asked to.
;;        press key sequence `C-c i', trigger `wlc/indent-context' to do this.
;;   Anyway, that is maximum in my head to do "indent things automatically
;;   (or smartly)" in lisp.  Any comment or suggestion on enhancing this
;;   feature is appreciated.
;;
;; 3. automatically complete the right part of pairs
;;   A piece of configure code to use skeleton.el library to complete the
;;   right part of pairs in Lisp, such as `', (), {} automatically when
;;   the left part of it is input.
;;
;; 4. show matching parentheses on point (cursor)
;;   A piece of configure code to use paren.el library to highlight
;;   the matching parentheses on insert point (cursor).
;;
;; 5. additional highlighting
;;   currently support addtional highlight on elisp keywords,
;;   built-in functions.
;;
;; Hope you like it!

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; requires

;; require part of `cc-mode' implementation as the base of hungry delete feature
(require 'cc-cmds)

;; require `font-lock' to utilize face property in `wlc/fast-in-literal'
(require 'font-lock)

;; require `skeleton' to complete the pairs
(require 'skeleton)

;; require `paren' to show matching parentheses on point
(require 'paren)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group defination

(defgroup wlc nil
  "Wonderful Lisp Coding extension."
  :group 'convenience
  :prefix "wlc/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization points

(defcustom wlc/all-features-on-mode-hook-list
  `(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook)
  "*A list of Lisp mode hooks to enable all features of `wlc'."
  :type 'list
  :group 'wlc)


(defcustom wlc/maximum-decoration-mode-list
  `(emacs-lisp-mode lisp-mode lisp-interaction-mode)
  "*A list of Lisp modes to enable maximum decoration (highlighting) feature."
  :type 'list
  :group 'wlc)


(defcustom wlc/backspace-function 'backward-delete-char-untabify
  "*Function called by `wlc/hungry-backspace' when deleting backwards."
  :type 'function
  :group 'wlc)


(defcustom wlc/delete-function 'delete-char
  "*Function called by `wlc/hungry-delete-forward' when deleting forwards."
  :type 'function
  :group 'wlc)


(defcustom wlc/defun-regexp "^\\s-*\\s(\\s-*def\\(?:un\\|un\\*\\|ine\\|\
macro\\|macro\\*\\|type\\|var\\|alias\\|varalias\\|const\\|custom\\|\
face\\|group\\|advice\\|subst\\|theme\\|struct\\|subst\\*\\|setf\\)\\s-+?"

  "*A regexp to match the start of a defun.
Always non-nil.  It could be customized to match the start of a defun
of another pattern on your need.
Used in function `wlc/beginning-of-defun'."
  :type '(choice (const nil)
                 regexp)
  :group 'wlc)


;; elisp keywords
(defcustom wlc/font-lock-keywords-elisp
  '(
    ;; short keywords
    ("\\_<\\(nil\\|t\\)\\_>" . font-lock-keyword-face)

    ;; built-in special form
    ("([ \t\n]*\\(\
let\\*\\|or\\|if\\|and\\|let\\|setq-default\\|setq\\|defun\\|\
function\\|interactive\\|condition-case\\|cond\\|defvar\\|while\\|\
progn\\|save-excursion\\|quote\\|defconst\\|defmacro\\|catch\\|\
save-restriction\\|eval-when-compile\\|prog1\\|unwind-protect\\|\
save-window-excursion\\|eval-and-compile\\|with-output-to-temp-buffer\\|\
with-no-warnings\\|save-current-buffer\\|track-mouse\\|prog2\\|count-loop\
\\)\\_>"
     1 'wlc/built-in-sfrom-face)

    ;; built-in functions that donot need argument
    ("([ \t\n]*\\(\
point-min-marker\\|point-min\\|point-max-marker\\|point-max\\|\
point-marker\\|point\\|current-buffer\\|bolp\\|syntax-table\\|\
current-column\\|selected-window\\|eobp\\|current-time\\|\
following-char\\|selected-frame\\|interactive-p\\|bobp\\|preceding-char\\|\
command-line\\|eolp\\|buffer-string\\|region-end\\|make-marker\\|\
current-indentation\\|kill-all-local-variables\\|current-message\\|\
region-beginning\\|recursive-edit\\|deactivate-mark\\|help-buffer\\|\
called-interactively-p\\|minibuffer-prompt-end\\|minibuffer-prompt-width\\|\
minibuffer-prompt\\|face-list\\|mark-marker\\|system-name\\|\
sentence-end\\|this-command-keys-vector\\|this-command-keys\\|\
current-local-map\\|visited-file-modtime\\|frame-list\\|mouse-position\\|\
input-pending-p\\|undo-boundary\\|barf-if-buffer-read-only\\|\
default-file-modes\\|current-global-map\\|minibuffer-depth\\|\
standard-syntax-table\\|process-list\\|make-auto-save-file-name\\|\
category-table\\|user-uid\\|minibuffer-contents-no-properties\\|\
minibuffer-contents\\|active-minibuffer-window\\|use-region-p\\|\
current-left-margin\\|recursion-depth\\|mouse-pixel-position\\|\
make-display-table\\|recent-keys\\|indent-to-left-margin\\|emacs-pid\\|\
current-fill-column\\|user-real-login-name\\|discard-input\\|\
backward-prefix-chars\\|standard-case-table\\|max-char\\|\
clear-visited-file-modtime\\|unlock-buffer\\|current-justification\\|\
backup-buffer\\|delete-minibuffer-contents\\|pop-mark\\|terminal-list\\|\
get-load-suffixes\\|visible-frame-list\\|recent-auto-save-p\\|\
current-input-mode\\|current-frame-configuration\\|user-real-uid\\|\
current-idle-time\\|describe-prefix-bindings\\|current-case-table\\|\
x-display-list\\|waiting-for-user-input-p\\|set-buffer-auto-saved\\|\
minibuffer-selected-window\\|minibuffer-completion-contents\\|\
memory-use-counts\\|memory-limit\\|make-category-table\\|\
list-system-processes\\|hack-dir-local-variables\\|current-minor-mode-maps\\|\
standard-category-table\\|rename-auto-save-file\\|handle-shift-selection\\|\
network-interface-list\\|get-internal-run-time\\|get-file-char\\|gap-size\\|\
gap-position\
\\)[ \t\n]*)"
     1 'wlc/built-in-function-nonarg-face)

    ;; built-in functins that do need at least one argument
    ("([ \t\n]*\\(\
\\*\\|-\\|set-buffer-modified-p\\|set-buffer-multibyte\\|\
set-buffer-major-mode\\|set-buffer\\|setcdr\\|setcar\\|\
set-marker-insertion-type\\|set-marker\\|set-mark\\|\
set-default-file-modes\\|set-default\\|set-syntax-table\\|\
set-keymap-parent\\|set-window-start\\|set-process-filter\\|\
set-text-properties\\|set-window-configuration\\|\
set-window-buffer\\|set-window-point\\|set-case-syntax-pair\\|\
set-case-syntax-delims\\|set-case-syntax\\|set-process-sentinel\\|\
set-match-data\\|set-window-dedicated-p\\|set-face-background\\|\
set-process-query-on-exit-flag\\|set-register\\|set-face-attribute\\|\
set-char-table-range\\|set-face-foreground\\|set-process-coding-system\\|\
set-visited-file-modtime\\|set-auto-coding\\|set-window-hscroll\\|\
set-mouse-position\\|set-process-buffer\\|set-auto-mode\\|\
set-char-table-extra-slot\\|set-face-font\\|setplist\\|set-file-times\\|\
set-terminal-parameter\\|set-window-vscroll\\|set-time-zone-rule\\|\
set-frame-width\\|set-frame-parameter\\|set-fontset-font\\|\
set-char-table-parent\\|set-window-margins\\|set-frame-size\\|\
set-face-underline-p\\|set-display-table-slot\\|set-frame-height\\|\
set-frame-position\\|set-coding-system-priority\\|set-window-display-table\\|\
set-input-mode\\|set-frame-selected-window\\|set-frame-configuration\\|\
set-face-bold-p\\|set-standard-case-table\\|set-face-inverse-video-p\\|\
set-case-table\\|set-window-fringes\\|set-process-plist\\|\
set-face-stipple\\|set-charset-priority\\|set-category-table\\|\
set-minibuffer-window\\|set-face-italic-p\\|set-window-scroll-bars\\|\
set-window-parameter\\|set-process-datagram-address\\|\
set-network-process-option\\|set-mouse-pixel-position\\|\
set-fringe-bitmap-face\\|set\\|listp\\|listify-key-sequence\\|\
list-fonts\\|list\\|modify-frame-parameters\\|modify-category-entry\\|\
modify-all-frames-parameters\\|mod\\|string-match-p\\|string-match\\|\
stringp\\|string=\\|string-to-number\\|string-equal\\|string<\\|\
string-to-char\\|string-lessp\\|string-width\\|string-as-unibyte\\|\
string-to-multibyte\\|string-or-null-p\\|string-to-syntax\\|\
string-as-multibyte\\|string-to-unibyte\\|string-bytes\\|string\\|\
not\\|expand-file-name\\|expt\\|exp\\|equal-including-properties\\|\
equal\\|eql\\|eq\\|>=\\|>\\|/=\\|/\\|<=\\|<\\|get-buffer-create\\|\
get-buffer-window-list\\|get-buffer-window\\|get-buffer-process\\|\
get-buffer\\|get-text-property\\|gethash\\|get-file-buffer\\|\
get-process\\|get-char-property-and-overlay\\|get-char-property\\|\
get-register\\|get-char-code-property\\|get-largest-window\\|\
get-charset-property\\|get-lru-window\\|get-window-with-predicate\\|\
get-byte\\|get-device-terminal\\|get-unused-category\\|get\\|round\\|\
car-safe\\|car\\|minibuffer-window-active-p\\|minibuffer-window\\|\
minibuffer-message\\|minibufferp\\|minor-mode-key-binding\\|min\\|\
consp\\|constrain-to-field\\|cons\\|=\\|ding\\|put-text-property\\|\
puthash\\|put-char-code-property\\|put-image\\|put-charset-property\\|\
put\\|\\+\\|insert-file-contents-literally\\|insert-file-contents\\|\
insert-buffer-substring-no-properties\\|insert-buffer-substring-as-yank\\|\
insert-buffer-substring\\|insert-char\\|insert-directory\\|insert-button\\|\
insert-before-markers-and-inherit\\|insert-before-markers\\|insert-image\\|\
insert-and-inherit\\|insert-text-button\\|insert-for-yank\\|\
insert-abbrev-table-description\\|insert-sliced-image\\|insert\\|\
message-box\\|message-or-box\\|message\\|%\\|marker-position\\|\
marker-buffer\\|markerp\\|marker-insertion-type\\|mark\\|\
read-string\\|read-file-name\\|read-command\\|read-from-minibuffer\\|\
read-char-exclusive\\|read-char\\|read-event\\|read-buffer\\|\
read-from-string\\|read-key-sequence-vector\\|read-key-sequence\\|\
read-passwd\\|read-directory-name\\|read-kbd-macro\\|read-regexp\\|\
read-variable\\|read-coding-system\\|read-shell-command\\|read-minibuffer\\|\
read-quoted-char\\|read-input-method-name\\|read-no-blanks-input\\|\
read-file-modes\\|read-non-nil-coding-system\\|read\\|load-average\\|\
load\\|format-time-string\\|format-mode-line\\|format-network-address\\|\
format-seconds\\|format\\|cdr-safe\\|cdr\\|error-message-string\\|\
error\\|nthcdr\\|nth\\|single-key-description\\|sin\\|concat\\|\
define-key-after\\|define-key\\|tan\\|logand\\|logior\\|log10\\|\
logxor\\|logb\\|lognot\\|log\\|last-buffer\\|last\\|autoload\\|\
delete-overlay\\|delete-process\\|delete-dups\\|delete-and-extract-region\\|\
delete-auto-save-file-if-necessary\\|delete-to-left-margin\\|\
delete-terminal\\|delete-field\\|delete\\|debug\\|max\\|length\\|\
1-\\|require\\|print\\|substring-no-properties\\|substring\\|\
eval-after-load\\|eval-minibuffer\\|eval\\|ash\\|ignore\\|remove-hook\\|\
remove-text-properties\\|remove-overlays\\|remove-from-invisibility-spec\\|\
remove-list-of-text-properties\\|remove-images\\|remove\\|looking-at-p\\|\
looking-at\\|member-ignore-case\\|member\\|1\\+\\|assoc-string\\|\
assoc-default\\|assoc\\|mapcar\\|mapconcat\\|mapc\\|memql\\|memq\\|\
sort-subr\\|sort\\|intern-soft\\|intern\\|null\\|aref\\|boundp\\|\
match-string-no-properties\\|match-string\\|documentation-property\\|\
documentation\\|append\\|provide\\|abs\\|match-end\\|funcall\\|\
match-beginning\\|elt\\|vectorp\\|vector\\|byte-compile\\|foo\\|\
apply-partially\\|apply\\|fset\\|reverse\\|warn\\|buffer-name\\|\
buffer-substring-no-properties\\|buffer-substring\\|assq-delete-all\\|\
assq\\|fboundp\\|princ\\|throw\\|add-hook\\|floatp\\|float-time\\|\
float\\|defalias\\|cadr\\|aset\\|signal-process\\|signal\\|featurep\\|\
buffer-file-name\\|nreverse\\|symbol-name\\|add-to-list\\|zerop\\|\
replace-match\\|file-exists-p\\|symbolp\\|integerp\\|symbol-value\\|\
regexp-quote\\|downcase\\|skip-chars-forward\\|numberp\\|run-hooks\\|\
match-data\\|delq\\|char-after\\|identity\\|make-sparse-keymap\\|\
file-name-nondirectory\\|nconc\\|select-window\\|window-height\\|\
y-or-n-p-with-timeout\\|y-or-n-p\\|overlay-put\\|file-name-directory\\|\
completing-read\\|buffer-modified-p\\|truncate-string-to-width\\|\
truncate\\|caar\\|plist-get\\|buffer-list\\|prin1-to-string\\|prin1\\|\
skip-chars-backward\\|make-string\\|sit-for\\|call-interactively\\|\
random\\|prefix-numeric-value\\|framep\\|split-string-and-unquote\\|\
split-string\\|file-directory-p\\|propertize\\|window-buffer\\|\
atom\\|process-buffer\\|copy-sequence\\|upcase-initials\\|upcase\\|\
cddr\\|buffer-live-p\\|char-to-string\\|symbol-function\\|cos\\|\
regexp-opt-depth\\|regexp-opt\\|frame-parameters\\|frame-parameter\\|\
window-start\\|key-binding\\|make-symbol\\|number-to-string\\|\
redisplay\\|line-end-position\\|functionp\\|file-attributes\\|\
find-file-noselect\\|line-beginning-position\\|sqrt\\|char-before\\|\
floor\\|call-process-region\\|call-process-shell-command\\|\
call-process\\|file-name-as-directory\\|count-lines\\|asin\\|\
cdar\\|make-overlay\\|use-local-map\\|lsh\\|add-text-properties\\|\
directory-files-and-attributes\\|directory-files\\|all-completions\\|\
make-vector\\|capitalize\\|buffer-size\\|window-width\\|\
window-system\\|file-readable-p\\|move-marker\\|overlay-get\\|\
run-hook-with-args-until-success\\|run-hook-with-args-until-failure\\|\
run-hook-with-args\\|lookup-key\\|ring-p\\|overlay-start\\|\
replace-regexp-in-string\\|beep\\|generate-new-buffer-name\\|\
generate-new-buffer\\|face-attribute-relative-p\\|face-attribute\\|\
byte-code-function-p\\|byte-code\\|process-filter\\|process-status\\|\
make-obsolete-variable\\|make-obsolete\\|push-mark\\|\
select-frame-set-input-focus\\|select-frame\\|file-truename\\|\
macroexpand-all\\|macroexpand\\|default-value\\|overlay-end\\|\
process-mark\\|next-single-property-change\\|directory-file-name\\|\
event-start\\|force-mode-line-update\\|cancel-timer\\|try-completion\\|\
parse-partial-sexp\\|define-abbrev-table\\|define-abbrev\\|\
syntax-ppss-flush-cache\\|syntax-ppss-toplevel-pos\\|syntax-ppss\\|\
process-coding-system\\|move-overlay\\|file-name-sans-extension\\|\
copy-marker\\|keymap-parent\\|yes-or-no-p\\|substitute-command-keys\\|\
bufferp\\|file-modes-symbolic-to-number\\|file-modes\\|\
abbreviate-file-name\\|start-process-shell-command\\|start-process\\|\
posn-window\\|window-live-p\\|make-hash-table\\|process-send-string\\|\
window-point\\|plist-put\\|window-frame\\|unsafep\\|make-frame\\|\
char-equal\\|run-mode-hooks\\|make-temp-file\\|char-syntax\\|\
event-end\\|coding-system-priority-list\\|coding-system-p\\|\
frame-width\\|font-spec\\|terpri\\|user-login-name\\|thing-at-point\\|\
purecopy\\|current-time-string\\|accept-process-output\\|\
make-syntax-table\\|vconcat\\|overlays-at\\|executable-find\\|\
current-window-configuration\\|posn-point\\|char-width\\|user-full-name\\|\
pos-visible-in-window-p\\|substitute-key-definition\\|shell-quote-argument\\|\
makunbound\\|frame-height\\|facep\\|md5\\|window-dedicated-p\\|\
maphash\\|local-variable-p\\|frame-live-p\\|forward-comment\\|\
substitute-in-file-name\\|face-background\\|skip-syntax-backward\\|\
process-sentinel\\|keymapp\\|skip-syntax-forward\\|processp\\|\
encode-coding-string\\|key-description\\|ceiling\\|encode-time\\|\
make-face\\|file-writable-p\\|button-type-get\\|button-type-subtype-p\\|\
button-type-put\\|button-type\\|decode-coding-string\\|vertical-motion\\|\
copy-keymap\\|make-keymap\\|window-list\\|process-name\\|mapatoms\\|\
find-buffer-visiting\\|ring-insert-at-beginning\\|ring-insert\\|\
window-edges\\|file-relative-name\\|make-list\\|locate-file\\|\
file-name-absolute-p\\|text-property-any\\|suppress-keymap\\|\
display-completion-list\\|create-image\\|ring-length\\|\
other-buffer\\|rassoc\\|overlays-in\\|file-name-extension\\|\
defvaralias\\|font-lock-add-keywords\\|commandp\\|file-remote-p\\|\
file-name-all-completions\\|booleanp\\|sleep-for\\|kill-new\\|\
batch-byte-compile\\|previous-single-property-change\\|open-network-stream\\|\
find-image\\|rassq-delete-all\\|rassq\\|one-window-p\\|window-hscroll\\|\
buffer-local-value\\|subst-char-in-region\\|convert-standard-filename\\|\
walk-windows\\|invisible-p\\|face-foreground\\|make-button\\|\
terminal-parameters\\|terminal-parameter\\|keywordp\\|window-minibuffer-p\\|\
ring-ref\\|x-popup-menu\\|edebug-trace\\|scan-lists\\|overlay-buffer\\|\
display-graphic-p\\|overlayp\\|image-type-available-p\\|window-end\\|\
compare-strings\\|process-id\\|file-newer-than-file-p\\|\
hack-local-variables\\|time-less-p\\|remhash\\|kill-process\\|\
copy-syntax-table\\|ring-empty-p\\|atan\\|play-sound-file\\|play-sound\\|\
looking-back\\|copy-face\\|characterp\\|ring-size\\|file-regular-p\\|\
display-warning\\|coding-system-get\\|process-query-on-exit-flag\\|\
map-charset-chars\\|event-modifiers\\|decode-time\\|button-get\\|\
posn-col-row\\|hash-table-p\\|copy-tree\\|unintern\\|scan-sexps\\|\
field-end\\|symbol-file\\|file-symlink-p\\|color-values\\|buffer-end\\|\
current-time-zone\\|butlast\\|tool-bar-add-item-from-menu\\|\
tool-bar-add-item\\|plist-member\\|interactive-form\\|\
event-basic-type\\|current-kill\\|char-table-extra-slot\\|\
define-button-type\\|buffer-base-buffer\\|file-name-completion\\|\
define-category\\|make-temp-name\\|file-executable-p\\|image-size\\|\
decode-char\\|where-is-internal\\|file-local-copy\\|face-font\\|\
char-table-range\\|text-property-not-all\\|process-file-shell-command\\|\
process-file\\|line-number-at-pos\\|help-setup-xref\\|\
encode-char\\|restore-buffer-modified-p\\|windowp\\|eventp\\|\
default-boundp\\|acos\\|keyboard-translate\\|current-word\\|clrhash\\|\
keyboard-coding-system\\|indirect-function\\|file-name-sans-versions\\|\
file-accessible-directory-p\\|ewoc-data\\|coding-system-eol-type\\|\
terminal-coding-system\\|lock-buffer\\|frame-selected-window\\|\
arrayp\\|verify-visited-file-modtime\\|tool-bar-local-item-from-menu\\|\
fillarray\\|execute-kbd-macro\\|copy-alist\\|time-to-days\\|\
subrp\\|shell-command-to-string\\|add-to-history\\|fmakunbound\\|\
x-get-selection\\|type-of\\|make-network-process\\|frame-visible-p\\|\
display-color-p\\|buffer-modified-tick\\|text-properties-at\\|\
frame-char-width\\|symbol-plist\\|start-file-process-shell-command\\|\
start-file-process\\|process-get\\|frame-char-height\\|\
add-to-invisibility-spec\\|find-file-name-handler\\|seconds-to-time\\|\
make-ring\\|make-char-table\\|button-label\\|ring-remove\\|remq\\|\
next-overlay-change\\|display-color-cells\\|defined-colors\\|\
date-to-time\\|color-defined-p\\|field-string-no-properties\\|\
field-string\\|coding-system-change-eol-conversion\\|char-table-parent\\|\
char-table-p\\|special-display-popup-frame\\|special-display-p\\|\
font-lock-remove-keywords\\|display-pixel-width\\|display-images-p\\|\
command-execute\\|char-displayable-p\\|process-put\\|map-keymap\\|\
event-click-count\\|display-pixel-height\\|charsetp\\|\
user-variable-p\\|process-send-eof\\|multibyte-string-p\\|\
coding-system-list\\|buffer-local-variables\\|number-or-marker-p\\|\
process-command\\|overlay-properties\\|next-single-char-property-change\\|\
frame-first-window\\|clear-string\\|window-vscroll\\|sequencep\\|nlistp\\|\
count-screen-lines\\|bindat-get-field\\|ad-add-advice\\|use-global-map\\|\
select-safe-coding-system\\|next-button\\|find-backup-file-name\\|\
display-table-slot\\|create-file-buffer\\|interrupt-process\\|\
imenu-add-to-menubar\\|window-full-width-p\\|time-subtract\\|\
test-completion\\|previous-overlay-change\\|field-beginning\\|\
clear-this-command-keys\\|window-inside-edges\\|process-exit-status\\|\
hash-table-count\\|send-string-to-terminal\\|process-contact\\|fround\\|\
find-coding-systems-region\\|filter-buffer-substring\\|ewoc-next\\|\
x-get-resource\\|syntax-table-p\\|posn-at-point\\|detect-coding-region\\|\
char-category-set\\|syntax-after\\|make-text-button\\|\
file-expand-wildcards\\|file-chase-links\\|face-underline-p\\|\
ewoc-locate\\|custom-reevaluate-setting\\|check-coding-systems-region\\|\
check-coding-system\\|window-fringes\\|parse-colon-path\\|\
mouse-movement-p\\|map-char-table\\|glyph-face\\|ffloor\\|window-margins\\|\
unibyte-char-to-multibyte\\|time-add\\|process-kill-without-query\\|\
posn-x-y\\|next-frame\\|make-backup-file-name\\|local-variable-if-set-p\\|\
display-mouse-p\\|define-fringe-bitmap\\|command-remapping\\|\
char-charset\\|tty-color-define\\|perform-replace\\|frame-terminal\\|\
ewoc-nth\\|add-to-ordered-list\\|window-tree\\|process-send-region\\|\
process-attributes\\|image-load-path-for-library\\|frame-pixel-width\\|\
force-window-update\\|charset-plist\\|base64-decode-region\\|\
unhandled-file-name-directory\\|tty-color-alist\\|subr-arity\\|\
posn-object-x-y\\|posn-object-width-height\\|posn-object\\|\
make-byte-code\\|global-key-binding\\|file-ownership-preserved-p\\|\
face-bold-p\\|backup-file-name-p\\|ask-user-about-supersession-threat\\|\
abbrev-symbol\\|x-server-vendor\\|wholenump\\|terminal-name\\|\
split-window-sensibly\\|scroll-bar-scale\\|safe-length\\|redraw-frame\\|\
previous-single-char-property-change\\|previous-frame\\|posn-string\\|\
next-char-property-change\\|map-y-or-n-p\\|integer-or-marker-p\\|\
glyph-char\\|frame-pixel-height\\|face-documentation\\|ewoc-invalidate\\|\
define-prefix-command\\|create-glyph\\|button-at\\|ask-user-about-lock\\|\
after-find-file\\|abbrev-expansion\\|x-list-fonts\\|terminal-live-p\\|\
next-property-change\\|make-glyph-code\\|kill-append\\|indirect-variable\\|\
font-get\\|font-at\\|ewoc-map\\|button-start\\|ad-define-subr-args\\|\
x-get-cut-buffer\\|text-char-description\\|syntax-class\\|\
stop-process\\|progress-reporter-update\\|make-progress-reporter\\|\
lwarn\\|find-charset-region\\|face-remap-set-base\\|\
create-fontset-from-fontset-spec\\|coordinates-in-window-p\\|\
button-end\\|buffer-chars-modified-tick\\|auto-save-file-name-p\\|\
x-popup-dialog\\|same-window-p\\|primitive-undo\\|position-bytes\\|\
multibyte-char-to-unibyte\\|momentary-string-display\\|make-abbrev-table\\|\
file-locked-p\\|face-italic-p\\|coding-system-change-text-conversion\\|\
base64-decode-string\\|backtrace-debug\\|window-inside-pixel-edges\\|\
window-display-table\\|quit-process\\|overlay-recenter\\|\
keymap-prompt\\|hash-table-size\\|find-operation-coding-system\\|\
fill-context-prefix\\|face-remap-reset-base\\|face-remap-add-relative\\|\
face-differs-from-default-p\\|ewoc-enter-before\\|display-popup-menus-p\\|\
display-grayscale-p\\|clear-image-cache\\|char-or-string-p\\|\
abbrev-table-put\\|abbrev-table-p\\|abbrev-table-get\\|abbrev-get\\|\
x-parse-geometry\\|window-configuration-p\\|suspend-tty\\|ring-copy\\|\
process-type\\|posn-timestamp\\|nbutlast\\|make-bool-vector\\|\
local-key-binding\\|jit-lock-register\\|face-inverse-video-p\\|\
ewoc-goto-node\\|display-mm-height\\|date-leap-year-p\\|\
compare-buffer-substrings\\|x-server-version\\|window-pixel-edges\\|\
window-body-height\\|window-at\\|serial-process-configure\\|\
process-lines\\|prepare-change-group\\|number-sequence\\|\
mouse-on-link-p\\|jit-lock-unregister\\|face-remap-remove-relative\\|\
ewoc-prev\\|ewoc-collect\\|display-visual-class\\|display-planes\\|\
display-mm-width\\|backtrace-frame\\|x-open-connection\\|\
tq-enqueue\\|tq-create\\|time-to-day-in-year\\|posn-actual-col-row\\|\
make-translation-table-from-alist\\|make-translation-table-from-vector\\|\
make-translation-table\\|makehash\\|fceiling\\|face-id\\|\
ewoc-goto-prev\\|ewoc-enter-last\\|ewoc-delete\\|ewoc-create\\|\
describe-display-table\\|decode-coding-inserted-region\\|\
continue-process\\|charset-after\\|adjust-window-trailing-edge\\|\
window-line-height\\|tty-color-clear\\|store-substring\\|ring-elements\\|\
redirect-frame-focus\\|progress-reporter-done\\|process-plist\\|\
previous-char-property-change\\|locale-info\\|lax-plist-get\\|\
ftruncate\\|fontp\\|find-coding-systems-string\\|find-auto-coding\\|\
ewoc-enter-after\\|edit-and-eval-command\\|current-active-maps\\|\
compute-motion\\|completion-table-dynamic\\|combine-and-quote-strings\\|\
coding-system-aliases\\|char-code-property-description\\|\
cancel-change-group\\|activate-change-group\\|access-file\\|\
accept-change-group\\|x-set-cut-buffer\\|tty-color-translate\\|\
transpose-regions\\|sxhash\\|risky-local-variable-p\\|fetch-bytecode\\|\
face-stipple\\|face-equal\\|ewoc-set-hf\\|ewoc-goto-next\\|ewoc-filter\\|\
display-screens\\|display-save-under\\|display-message-or-buffer\\|\
display-backing-store\\|detect-coding-string\\|coding-system-charset-list\\|\
button-put\\|button-activate\\|bindat-unpack\\|bindat-length\\|\
window-scroll-bars\\|unibyte-string\\|process-tty-name\\|posn-at-x-y\\|\
lax-plist-put\\|invert-face\\|image-mask-p\\|fringe-bitmaps-at-pos\\|\
find-coding-systems-for-charsets\\|file-nlinks\\|file-newest-backup\\|\
ewoc-location\\|ewoc-get-hf\\|ewoc-enter-first\\|ewoc-buffer\\|\
display-supports-face-attributes-p\\|display-selections-p\\|\
dir-locals-set-directory-class\\|dir-locals-set-class-variables\\|\
color-supported-p\\|char-table-subtype\\|buffer-swap-text\\|\
x-close-connection\\|window-current-scroll-bars\\|\
tty-color-approximate\\|tq-close\\|Snarf-documentation\\|\
safe-local-variable-p\\|resume-tty\\|quietly-read-abbrev-file\\|\
progress-reporter-force-update\\|previous-property-change\\|\
previous-button\\|posn-image\\|posn-area\\|posix-string-match\\|\
match-substitute-replacement\\|make-serial-process\\|image-refresh\\|\
hash-table-test\\|frame-current-scroll-bars\\|font-xlfd-name\\|\
font-face-attributes\\|ewoc-set-data\\|ewoc-refresh\\|event-convert-list\\|\
copy-hash-table\\|clear-abbrev-table\\|category-set-mnemonics\\|\
case-table-p\\|buffer-has-markers-at\\|bool-vector-p\\|bindat-pack\\|\
bindat-ip-to-string\\|accessible-keymaps\\|abbrev-put\\|write-char\\|\
scroll-bar-event-ratio\\|merge-face-attribute\\|hash-table-weakness\\|\
font-put\\|font-family-list\\|find-charset-string\\|face-all-attributes\\|\
dump-emacs\\|custom-add-frequent-value\\|copy-abbrev-table\\|\
controlling-tty-p\\|compare-window-configurations\\|category-docstring\\|\
byte-to-position\\|button-has-type-p\\|x-family-fonts\\|window-parameters\\|\
window-parameter\\|window-configuration-frame\\|process-running-child-p\\|\
process-datagram-address\\|posix-looking-at\\|network-interface-info\\|\
make-category-set\\|hash-table-rehash-threshold\\|hash-table-rehash-size\\|\
find-font\\|destroy-fringe-bitmap\\|define-hash-table-test\\|\
copy-category-table\\|color-gray-p\\|charset-priority-list\\|\
category-table-p\\|bitmap-spec-p\
\\)\\_>"
     1 'wlc/built-in-function-arg-face)

    ;; built-in macros
    ("(\\s-*\\(\
when\\|defcustom\\|unless\\|pop\\|push\\|with-current-buffer\\|\
dolist\\|kbd\\|declare-function\\|declare\\|defgroup\\|defface\\|\
save-match-data\\|with-temp-buffer\\|ignore-errors\\|def-edebug-spec\\|\
dotimes-with-progress-reporter\\|dotimes\\|ad-get-args\\|ad-get-arg\\|\
define-obsolete-function-alias\\|define-minor-mode\\|define-derived-mode\\|\
save-selected-window\\|with-syntax-table\\|define-obsolete-variable-alias\\|\
1value\\|with-timeout\\|define-generic-mode\\|with-temp-file\\|\
with-selected-window\\|ad-set-args\\|ad-set-arg\\|with-output-to-string\\|\
defimage\\|define-globalized-minor-mode\\|noreturn\\|with-temp-message\\|\
delay-mode-hooks\\|with-help-window\\|with-local-quit\\|\
combine-after-change-calls\\|with-coding-priority\\|lazy-completion-table\\|\
while-no-input\\|make-help-screen\\|eval-at-startup\\|edebug-tracing\\|\
with-case-table\
\\)\\_>"
     1 'wlc/built-in-macro-face)

    ;; built-in commands
    ("(\\s-*\\(\
goto-char\\|search-forward\\|re-search-forward\\|make-local-variable\\|\
forward-line\\|beginning-of-line\\|newline-and-indent\\|newline\\|\
find-file-other-window\\|find-file-read-only\\|find-file\\|\
forward-char\\|delete-region\\|other-window\\|end-of-line\\|\
search-backward\\|kill-buffer\\|yank-pop\\|yank\\|modify-syntax-entry\\|\
make-variable-buffer-local\\|erase-buffer\\|switch-to-buffer-other-window\\|\
switch-to-buffer\\|re-search-backward\\|apropos\\|widen\\|backward-char\\|\
delete-char\\|narrow-to-region\\|forward-sexp\\|save-buffer\\|recenter\\|\
display-buffer\\|indent-to\\|up-list\\|delete-file\\|global-set-key\\|\
move-to-column\\|insert-buffer\\|top-level\\|undefined\\|fill-paragraph\\|\
forward-word\\|self-insert-command\\|universal-argument\\|write-file\\|\
write-region\\|back-to-indentation\\|split-window-vertically\\|\
split-window-horizontally\\|split-window\\|beginning-of-defun\\|\
load-file\\|goto-line\\|revert-buffer\\|scroll-up\\|delete-windows-on\\|\
delete-window\\|getenv\\|bury-buffer\\|make-directory\\|\
fill-region-as-paragraph\\|fill-region\\|delete-other-windows\\|\
scroll-down\\|emacs-version\\|backward-sexp\\|kill-region\\|end-of-defun\\|\
kill-emacs\\|buffer-disable-undo\\|load-library\\|exit-minibuffer\\|\
indent-region\\|describe-mode\\|copy-file\\|ad-activate-regexp\\|\
ad-activate-all\\|ad-activate\\|delete-horizontal-space\\|\
delete-backward-char\\|byte-compile-file\\|local-set-key\\|rename-file\\|\
backward-word\\|minibuffer-complete-and-exit\\|minibuffer-complete-word\\|\
minibuffer-complete\\|kill-local-variable\\|digit-argument\\|push-button\\|\
beginning-of-buffer\\|scroll-other-window\\|fundamental-mode\\|\
locate-library\\|end-of-buffer\\|toggle-read-only\\|debug-on-entry\\|\
backtrace\\|view-file\\|delete-frame\\|indent-according-to-mode\\|\
keyboard-quit\\|shrink-window-if-larger-than-buffer\\|\
shrink-window-horizontally\\|shrink-window\\|disassemble\\|\
scroll-left\\|raise-frame\\|down-list\\|scroll-right\\|expand-abbrev\\|\
eval-buffer\\|run-with-idle-timer\\|run-at-time\\|rename-buffer\\|\
enlarge-window-horizontally\\|enlarge-window\\|copy-region-as-kill\\|\
move-to-window-line\\|delete-directory\\|setenv\\|save-some-buffers\\|\
iconify-frame\\|set-file-modes\\|backward-delete-char-untabify\\|\
describe-bindings\\|Helper-help\\|eval-region\\|forward-list\\|\
fit-window-to-buffer\\|exit-recursive-edit\\|ad-deactivate-all\\|\
ad-deactivate-regexp\\|ad-deactivate\\|garbage-collect\\|\
set-visited-file-name\\|indent-rigidly\\|capitalize-word\\|\
negative-argument\\|ad-enable-advice\\|word-search-forward-lax\\|\
word-search-forward\\|sort-fields\\|ad-disable-advice\\|upcase-word\\|\
just-one-space\\|downcase-word\\|byte-recompile-directory\\|\
ad-update-regexp\\|ad-update-all\\|ad-update\\|make-symbolic-link\\|\
decode-coding-region\\|make-frame-visible\\|abort-recursive-edit\\|\
execute-extended-command\\|downcase-region\\|buffer-enable-undo\\|\
normal-mode\\|add-name-to-file\\|indent-relative-maybe\\|\
indent-relative\\|previous-history-element\\|next-history-element\\|\
fixup-whitespace\\|sort-numeric-fields\\|word-search-backward-lax\\|\
word-search-backward\\|minibuffer-completion-help\\|delete-blank-lines\\|\
auto-save-mode\\|append-to-file\\|ad-unadvise-all\\|ad-unadvise\\|\
balance-windows-area\\|balance-windows\\|upcase-region\\|not-modified\\|\
indent-for-tab-command\\|tab-to-tab-stop\\|suspend-emacs\\|\
move-to-left-margin\\|delete-indentation\\|base64-encode-string\\|\
ad-start-advice\\|unload-feature\\|narrow-to-page\\|insert-register\\|\
encode-coding-region\\|cancel-debug-on-entry\\|translate-region\\|\
sort-lines\\|make-indirect-buffer\\|ad-enable-regexp\\|\
compile-defun\\|global-unset-key\\|set-terminal-coding-system\\|\
reindent-then-newline-and-indent\\|make-frame-on-display\\|\
justify-current-line\\|set-keyboard-coding-system\\|x-set-selection\\|\
sort-pages\\|redraw-display\\|read-color\\|handle-switch-frame\\|\
blink-matching-open\\|backward-list\\|list-processes\\|\
Helper-describe-bindings\\|fill-individual-paragraphs\\|sort-regexp-fields\\|\
set-input-method\\|lower-frame\\|edebug-display-freq-count\\|\
capitalize-region\\|ad-stop-advice\\|make-frame-invisible\\|\
base64-encode-region\\|ad-disable-regexp\\|sort-columns\\|\
list-charset-chars\\|indent-code-rigidly\\|suspend-frame\\|\
describe-current-display-table\\|unbury-buffer\\|set-left-margin\\|\
next-matching-history-element\\|sort-paragraphs\\|self-insert-and-exit\\|\
replace-buffer-in-windows\\|previous-matching-history-element\\|\
local-unset-key\\|forward-button\\|backward-button\\|write-abbrev-file\\|\
forward-to-indentation\\|format-write-file\\|format-insert-file\\|\
enable-command\\|do-auto-save\\|set-right-margin\\|serial-term\\|\
emacs-uptime\\|emacs-init-time\\|disable-command\\|describe-categories\\|\
abbrev-prefix-mark\\|view-register\\|format-find-file\\|\
backward-to-indentation\\|open-dribble-file\\|describe-buffer-case-table\\|\
posix-search-forward\\|posix-search-backward\\|open-termscript\
\\)\\_>"
     1 'wlc/built-in-command-face)

    ;; built-in variables
    ("\\_<\\(\
values\\|major-mode\\|buffer-file-name\\|debugger\\|default-directory\\|\
font-lock-keywords-only\\|font-lock-keywords-case-fold-search\\|\
font-lock-keywords\\|kill-ring-yank-pointer\\|kill-ring\\|\
buffer-read-only\\|prefix-arg\\|last-command-event\\|\
last-command-char\\|last-command\\|current-prefix-arg\\|features\\|\
this-command\\|global-map\\|inhibit-read-only\\|mode-name\\|\
post-command-hook\\|obarray\\|system-type\\|mark-active\\|\
auto-mode-alist\\|buffer-undo-list\\|standard-output\\|\
font-lock-defaults\\|window-system\\|buffer-file-coding-system\\|\
unread-command-events\\|minor-mode-alist\\|noninteractive\\|emacs-version\\|\
command-line-args-left\\|command-line-args\\|coding-system-for-write\\|\
completion-ignore-case\\|pre-command-hook\\|buffer-invisibility-spec\\|\
tool-bar-map\\|imenu-generic-expression\\|ad-do-it\\|last-input-event\\|\
kill-buffer-hook\\|header-line-format\\|deactivate-mark-hook\\|\
deactivate-mark\\|emacs-major-version\\|coding-system-for-read\\|\
standard-display-table\\|file-name-coding-system\\|format-alist\\|\
command-history\\|after-change-functions\\|minor-mode-map-alist\\|\
inhibit-quit\\|charset-list\\|font-lock-syntactic-keywords\\|\
line-prefix\\|inhibit-point-motion-hooks\\|indent-line-function\\|\
mark-ring\\|ad-return-value\\|overwrite-mode\\|minibuffer-history\\|\
overlay-arrow-position\\|minibuffer-local-map\\|print-level\\|\
mode-line-process\\|mode-line-buffer-identification\\|\
process-environment\\|minibuffer-setup-hook\\|last-nonmenu-event\\|\
selective-display\\|auto-fill-function\\|print-length\\|data-directory\\|\
executing-kbd-macro\\|overriding-terminal-local-map\\|global-mode-string\\|\
local-abbrev-table\\|last-kbd-macro\\|help-map\\|current-input-method\\|\
imenu-create-index-function\\|change-major-mode-hook\\|print-circle\\|\
kill-emacs-hook\\|image-load-path\\|last-coding-system-used\\|\
load-history\\|before-change-functions\\|write-file-functions\\|\
fill-paragraph-function\\|file-name-history\\|cursor-in-echo-area\\|\
revert-buffer-function\\|default-process-coding-system\\|\
generated-autoload-file\\|minibuffer-completion-table\\|\
process-connection-type\\|parse-sexp-lookup-properties\\|\
unread-command-char\\|font-lock-multiline\\|overriding-local-map-menu-flag\\|\
overriding-local-map\\|minibuffer-local-completion-map\\|\
c-mode-syntax-table\\|byte-compile-dynamic-docstrings\\|\
byte-compile-dynamic\\|warning-series\\|interpreter-mode-alist\\|\
path-separator\\|input-method-function\\|quit-flag\\|user-init-file\\|\
defining-kbd-macro\\|default-enable-multibyte-characters\\|\
buffer-file-type\\|inhibit-file-name-handlers\\|help-form\\|\
buffer-auto-save-file-name\\|use-hard-newlines\\|regexp-history\\|\
inhibit-modification-hooks\\|minibuffer-completion-predicate\\|\
buffer-file-truename\\|beginning-of-defun-function\\|vc-mode\\|\
inhibit-field-text-motion\\|process-coding-system-alist\\|\
last-abbrev-location\\|last-abbrev-text\\|last-abbrev\\|\
emacs-lisp-mode-syntax-table\\|user-emacs-directory\\|\
input-decode-map\\|system-configuration\\|inhibit-file-name-operation\\|\
window-scroll-functions\\|print-escape-newlines\\|minibuffer-scroll-window\\|\
window-size-fixed\\|local-function-key-map\\|end-of-defun-function\\|\
temp-buffer-show-hook\\|imenu-case-fold-search\\|text-mode-syntax-table\\|\
standard-input\\|init-file-user\\|locale-coding-system\\|\
read-expression-history\\|overlay-arrow-string\\|list-buffers-directory\\|\
font-lock-syntactic-face-function\\|window-configuration-change-hook\\|\
normal-auto-fill-function\\|last-event-frame\\|interprogram-cut-function\\|\
buffer-file-format\\|after-init-hook\\|key-translation-map\\|\
keyboard-translate-table\\|mode-line-modified\\|\
interprogram-paste-function\\|write-contents-functions\\|\
shell-command-history\\|font-lock-fontify-region-function\\|\
emacs-minor-version\\|hack-local-variables-hook\\|\
translation-table-for-input\\|syntax-begin-function\\|\
initial-window-system\\|desktop-buffer-mode-handlers\\|\
most-positive-fixnum\\|minibuffer-local-must-match-map\\|load-suffixes\\|\
after-revert-hook\\|query-replace-map\\|kill-buffer-query-functions\\|\
indent-region-function\\|font-lock-beginning-of-syntax-function\\|\
buffer-display-table\\|load-in-progress\\|text-property-default-nonsticky\\|\
minor-mode-overriding-map-alist\\|left-margin-width\\|global-abbrev-table\\|\
desktop-save-buffer\\|warning-levels\\|undo-in-progress\\|\
menu-bar-update-hook\\|load-read-function\\|invocation-name\\|\
font-lock-mark-block-function\\|buffer-file-number\\|print-quoted\\|\
menu-bar-final-items\\|last-repeatable-command\\|input-method-alist\\|\
imenu-syntax-alist\\|font-lock-unfontify-region-function\\|\
emacs-build-time\\|backup-inhibited\\|register-alist\\|\
mouse-position-function\\|insert-directory-program\\|\
exec-directory\\|default-minibuffer-frame\\|char-width-table\\|\
write-region-annotate-functions\\|window-size-change-functions\\|\
this-original-command\\|imenu-prev-index-position-function\\|\
face-remapping-alist\\|emulation-mode-map-alists\\|abbrevs-changed\\|\
scroll-bar-width\\|minibuffer-exit-hook\\|\
file-local-variables-alist\\|buffer-backed-up\\|before-revert-hook\\|\
abbrev-start-location-buffer\\|abbrev-start-location\\|x-resource-name\\|\
imenu-extract-index-name-function\\|customize-package-emacs-version-alist\\|\
blink-paren-function\\|window-setup-hook\\|warning-prefix-function\\|\
warning-fill-prefix\\|other-window-scroll-buffer\\|\
minibuffer-completion-confirm\\|doc-directory\\|default-text-properties\\|\
after-make-frame-functions\\|activate-mark-hook\\|print-gensym\\|\
fringe-indicator-alist\\|font-lock-syntax-table\\|buffer-substring-filters\\|\
buffer-display-time\\|after-change-major-mode-hook\\|\
suspend-hook\\|special-event-map\\|num-input-keys\\|\
kill-emacs-query-functions\\|query-replace-history\\|purify-flag\\|\
minibuffer-local-ns-map\\|magic-mode-alist\\|invocation-directory\\|\
exec-suffixes\\|disabled-command-function\\|auto-fill-chars\\|\
term-setup-hook\\|save-buffer-coding-system\\|real-last-command\\|\
read-file-name-function\\|minor-mode-list\\|installation-directory\\|\
ignored-local-variables\\|generate-autoload-cookie\\|frame-title-format\\|\
find-file-not-found-functions\\|before-init-hook\\|auto-save-hook\\|\
after-insert-file-functions\\|abbrev-table-name-list\\|yank-undo-function\\|\
x-pointer-shape\\|temp-buffer-setup-hook\\|mode-line-frame-identification\\|\
minibuffer-allow-text-properties\\|message-truncate-lines\\|\
magic-fallback-mode-alist\\|load-file-rep-suffixes\\|\
font-lock-extend-after-change-region-function\\|char-property-alias-alist\\|\
buffer-saved-size\\|window-point-insertion-type\\|warning-type-format\\|\
tool-bar-button-margin\\|text-mode-abbrev-table\\|search-spaces-regexp\\|\
print-escape-nonascii\\|overlay-arrow-variable-list\\|minibuffer-help-form\\|\
lisp-mode-abbrev-table\\|image-types\\|gcs-done\\|gc-elapsed\\|\
font-lock-fontify-buffer-function\\|fontification-functions\\|\
float-output-format\\|dir-locals-directory-cache\\|char-script-table\\|\
before-make-frame-hook\\|after-load-alist\\|show-help-function\\|\
right-margin-width\\|mode-line-mule-info\\|icon-title-format\\|\
font-lock-extra-managed-props\\|command-switch-alist\\|\
backup-enable-predicate\\|timer-max-repeats\\|term-file-prefix\\|\
system-time-locale\\|split-string-default-separators\\|\
revert-buffer-insert-file-contents-function\\|print-number-table\\|\
network-coding-system-alist\\|max-image-size\\|face-font-rescale-alist\\|\
emacs-startup-hook\\|default-mode-line-format\\|completion-regexp-list\\|\
byte-boolean-vars\\|buffer-display-count\\|auto-save-list-file-name\\|\
memory-full\\|glyph-table\\|font-lock-unfontify-buffer-function\\|\
dir-locals-class-alist\\|command-line-processed\\|\
before-hack-local-variables-hook\\|suspend-resume-hook\\|\
replace-search-function\\|replace-re-search-function\\|read-circle\\|\
multi-query-replace-map\\|multibyte-syntax-as-symbol\\|\
minibuffer-confirm-exit-commands\\|left-fringe-width\\|\
last-prefix-arg\\|image-library-alist\\|fundamental-mode-abbrev-table\\|\
frame-inherited-parameters\\|default-fill-column\\|debug-on-next-call\\|\
completion-styles-alist\\|buffer-save-without-query\\|\
abbrev-expand-functions\\|unload-feature-special-hooks\\|\
tool-bar-button-relief\\|system-messages-locale\\|right-fringe-width\\|\
print-escape-multibyte\\|print-continuous-numbering\\|\
minibuffer-local-filename-completion-map\\|inhibit-null-byte-detection\\|\
history-add-new-input\\|fringes-outside-margins\\|\
fill-forward-paragraph-function\\|emacs-save-session-functions\\|\
command-line-functions\\|auto-window-vscroll\\|\
x-sensitive-text-pointer-shape\\|write-region-post-annotation-function\\|\
system-key-alist\\|minibuffer-local-shell-command-map\\|\
minibuffer-local-filename-must-match-map\\|last-input-char\\|\
kbd-macro-termination-hook\\|initial-environment\\|font-list-limit\\|\
first-change-hook\\|extended-command-history\\|echo-area-clear-hook\\|\
delete-terminal-functions\\|cache-long-line-scans\\|\
buffer-name-history\\|buffer-auto-save-file-format\\|\
ascii-case-table\\|abbrev-minor-mode-table-alist\\|\
tty-erase-char\\|standard-translation-table-for-encode\\|\
standard-translation-table-for-decode\\|redisplay-dont-pause\\|\
pure-bytes-used\\|process-adaptive-read-buffering\\|\
prefix-help-command\\|fringe-cursor-alist\\|disable-point-adjustment\\|\
default-header-line-format\\|buffer-access-fontify-functions\\|\
x-super-keysym\\|x-resource-class\\|x-meta-keysym\\|x-hyper-keysym\\|\
x-alt-keysym\\|wrap-prefix\\|void-text-area-pointer\\|vector-cells-consed\\|\
tool-bar-border\\|symbols-consed\\|strings-consed\\|string-chars-consed\\|\
ring-bell-function\\|redisplay-preemption-period\\|printable-chars\\|\
post-gc-hook\\|play-sound-functions\\|num-nonmacro-input-events\\|\
multiple-frames\\|most-negative-fixnum\\|misc-objects-consed\\|\
menu-prompt-more-char\\|intervals-consed\\|inhibit-iso-escape-detection\\|\
image-cache-eviction-delay\\|global-disable-point-adjustment\\|\
floats-consed\\|extra-keyboard-modifiers\\|electric-future-map\\|\
default-indicate-buffer-boundaries\\|default-fringes-cursor-alist\\|\
default-fringe-indicator-alist\\|default-ctl-arrow\\|\
default-case-fold-search\\|default-abbrev-mode\\|\
cons-cells-consed\\|command-error-function\\|command-debug-status\\|\
buffer-access-fontified-property\\|auto-resize-tool-bars\\|\
auto-raise-tool-bar-buttons\
\\)\\_>"
     1 'wlc/built-in-variable-face))

  "*A list of keywords of elisp.
A user-level font-lock keywords to be added to the end of `font-lock-keywords'.
Refer to `wlc/maximum-decoration-on' for its usage."
  :group 'wlc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Faces

(defface wlc/built-in-sfrom-face
  '((t :inherit font-lock-keyword-face))

  "Face to use for highlighting elisp built-in special form in font-lock mode."
  :group 'wlc)


(defface wlc/built-in-function-nonarg-face
  '((t :inherit font-lock-builtin-face))

  "Face to use for highlighting elisp built-in function (with no argument)
in font-lock mode."
  :group 'wlc)


(defface wlc/built-in-function-arg-face
  '((t :inherit font-lock-builtin-face))

  "Face to use for highlighting elisp built-in function (with argument)
in font-lock mode."
  :group 'wlc)


(defface wlc/built-in-command-face
  '((((class color) (min-colors 88)) (:foreground "#729FCF"))
    (((class color) (min-colors 16)) (:foreground "#729FCF"))
    (((class color) (min-colors 8)) (:foreground "blue"))
    (((type tty) (class mono)) (:foreground "blue"))
    (t (:inherit default :foreground "blue")))
  "Face to use for highlighting elisp built-in command in font-lock mode."
  :group 'wlc)


(defface wlc/built-in-macro-face
  '((((class color) (min-colors 88)) (:foreground "#00d8ff" :weight semi-bold))
    (((class color) (min-colors 16)) (:foreground "#00d8ff" :weight semi-bold))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (((type tty) (class mono)) (:foreground "cyan" :weight bold))
    (t (:inherit default :foreground "cyan" :weight bold)))
  "Face to use for highlighting elisp built-in macro in font-lock mode."
  :group 'wlc)


(defface wlc/built-in-variable-face
  '((((class color) (min-colors 88)) (:foreground "seagreen2"))
    (((class color) (min-colors 16)) (:foreground "seagreen2"))
    (((class color) (min-colors 8)) (:foreground "green" :weight bold))
    (((type tty) (class mono)) (:foreground "green" :weight bold))
    (t (:inherit default :foreground "green" :weight bold)))
  "Face to use for highlighting elisp built-in variable in font-lock mode."
  :group 'wlc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables

(defvar wlc/hungry-delete-key nil
  "Internal state of Lisp hungry delete key feature.
Toggled by `wlc/toggle-hungry-state'.")

(make-variable-buffer-local 'wlc/hungry-delete-key)


(defvar wlc/fast-delete-flag t
  "Internal flag to control whether the delete or backspace function
behaviors differently according to lisp syntax context, that is,
every time when delete or backspace happens, parse the syntax context.
If it is about to delete or backspace inside a string or comment, call
`wlc/delete-function' or `wlc/backspace-function' instead of
`wlc/hungry-delete-forward' or `wlc/hungry-delete-backwards'.

Turn this flag on if you feel delete or backspace is too slow under
wlc to avoid the expense of parsing the syntax on delete or
backspace.
See `wlc/toggle-fast-delete' for more information.")

(make-variable-buffer-local 'wlc/fast-delete-flag)


(defvar wlc/electric-flag t
  "Internal flag of electric indentation feature.
Toggled by `wlc/toggle-electric-state'. If t, electric actions
\(like automatic reindentation) will happen when an electric key
like `enter' (or in an another name `return') is pressed.")

(make-variable-buffer-local 'wlc/electric-flag)


(defvar wlc/auto-complete-state-init-flag nil
  "Internal flag to indicate automatically complete pair feature initialized.
It is set to t after the first time this feature is enabled and
keep the same value for current buffer ever.
It is created to keep the code executed minimum and improve the performance.
Never set it in any style.
See the function `wlc/toggle-auto-complete-pair'.")

(make-variable-buffer-local 'wlc/auto-complete-state-init-flag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions


;;; user interface
(defun wlc/toggle-fast-delete (&optional arg)
  "Toggle `wlc/fast-delete-flag' of `wlc/hungry-delete-key' feature.
Optional numeric ARG, if supplied, turns on fast delete flag when
positive, turns if off when negative, and just toggles it when zero or
left out.

See `wlc/fast-delete-flag' for more information."
  (interactive "P")
  (setq wlc/fast-delete-flag
        (c-calculate-state arg wlc/fast-delete-flag)))


;;; user interface
(defun wlc/toggle-hungry-state (&optional arg)
  "Toggle `wlc/hungry-delete-key' feature.
Optional numeric ARG, if supplied, turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero or left out.

When the `wlc/hungry-delete-key' feature is enabled (indicated by \"/h\"
on the modeline after the mode name) the delete key gobbles all preceding
whitespace in one fell swoop."
  (interactive "P")
  (setq wlc/hungry-delete-key
        (c-calculate-state arg wlc/hungry-delete-key)))


;;; user interface
(defun wlc/toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (setq wlc/electric-flag (c-calculate-state arg wlc/electric-flag))
  (wlc/electric-keybindings))


;;; user interface
(defun wlc/toggle-auto-complete-pair (&optional arg)
  "Toggle the automatically complete pair feature.
Optional numeric ARG, if supplied, turns on automatically complete pair when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (if (not wlc/auto-complete-state-init-flag)
      (progn
        (wlc/auto-complete-pair)
        (setq wlc/auto-complete-pair-state-init-flag t))
    (setq skeleton-pair (c-calculate-state arg skeleton-pair))))


;;; user interface
(defun wlc/toggle-show-paren-on-point (&optional arg)
  "Toggle the show matching parentheseses on point feature.
Optional numberic ARG, if supplied, turns on this feature when
positive, turns it off when negative, and just toggles it when zero
or left out.
When it is enabled, any matching parentheses is highlighted on point.
See the function `show-paren-mode' for more information."
  (interactive "P")
  (if (c-calculate-state arg show-paren-mode)
      (show-paren-mode 1)
    (show-paren-mode -1)))


(defun wlc/fast-in-literal (&optional position)
  "Return the type of literal pointer is in, if any, dedicated to Lisp modes.
The return value is `string' if in a string literal, `comment' if in a Lisp
style single line comment, or nil if somewhere else.
Optional POSITION is used as the position in current buffer to ask about.
If omitted or nil, current point indicated by function `point' is used.

This is a fast implementation based on font-lock face.  It requires you
to turn on font-lock mode on every buffer where you turn on
`wlc/fast-delete-flag' and want to delete or backspace things faster.
There is a slow implementation, `wlc/slow-in-literal', which doesn't
depend on the font-lock feature."
  (interactive "p")
  (or position (setq position (point)))
  (let ((property-name nil))
    (setq property-name (get-text-property position 'face))
    (cond
     ((equal property-name 'font-lock-string-face)
      'string)
     ((equal property-name 'font-lock-comment-face)
      'comment)
     (t
      nil))))


(defun wlc/slow-in-literal (&optional position)
  "Return the type of literal pointer is in, if any, dedicated to Lisp modes.
The return value is `string' if in a string literal, `comment' if in a Lisp
style single line comment, or nil if somewhere else.
Optional POSITION is used as the position in current buffer to ask about.
If omitted or nil, current point indicated by function `point' is used.

This is a slow implementation based on brutal syntax parsing.
After that I find a better way to do this using font-lock face, since
font-lock face is updated on real-time when it's on.
See `wlc/fast-in-literal' for more information."
  (interactive "p")
  (or position (setq position (point)))
  (save-excursion
    (save-restriction
      (let ((string-open nil)
            (comment-open nil)
            (keep-searching t))
        (widen)
        (goto-char (point-min))
        ;;
        ;; A lightweight state machine of embedded switch implementation
        ;; to parse current buffer in lisp mode.
        ;;
        ;; start from the beginning of current buffer, search every
        ;; non-escaped character ';' or '"' and record the state change.
        ;; (non-escaped character ';' or '"' plays the role of starting
        ;; the comment or string, or, ending the string in syntax table.)
        (while (and keep-searching
                    (re-search-forward
                     "[^\\\";]*\\(?:\\\\\\(?:.\\|\n\\)[^\\\";]*\\)*\\([\";]?\\)"
                     position 'MOVE-TO-LIMIT)
                    (= 1 (length (match-string 1))))
          (if (equal t (compare-strings (match-string 1) 0 1 ";" 0 1))
              ;; Branch 1. a non-escaped ';' character is found
              (progn
                (cond
                 ((equal string-open nil) ;; point is not inside a string
                  (setq comment-open t)
                  ;; search the end of current line (Don't follow
                  ;; line continuation, which uses escape character '\' to
                  ;; escape new-line '\n' character. Because lisp single line
                  ;; comment doesn't support line continuation.)
                  (if (and (re-search-forward "[^\n]*\n"
                                              position 'MOVE-TO-LIMIT))
                      ;; reach the end of line, single line comment is close now
                      (setq comment-open nil)
                    ;; cannot find the end of line. We reach `position' already.
                    (setq keep-searching nil)))
                 (t ;; point is inside a string, continue to
                  ;; search the ending string non-escaped '"' character
                  )))
            ;; Branch 2. a non-escaped '"' character is found
            (progn
              (cond
               ((equal comment-open nil) ;; point is not inside a comment
                )
               (t ;; point is inside a comment?!
                ;; The executive flow is impossible to fall in here
                ;; except the code is wrong, because we skip over
                ;; every single line comment in the branch 1
                (error "something is fatally wrong.")))
              (if (equal string-open nil)
                  (setq string-open t)
                (setq string-open nil)))))
        (cond
         ((equal string-open t) ;; position is inside a string
          'string)
         ((equal comment-open t) ;; position is inside a comment
          'comment)
         (t ;; position is neither in a string or a comment
          nil))))))


(defun wlc/in-literal (&optional position)
  "Return the type of literal point is in, if any, dedicated to Lisp modes.
The return value is `string' if in a string literal, `comment' if in a Lisp
style single line comment, or nil if somewhere else.
Optional POSITION is used as the position in current buffer to ask about.
If omitted or nil, current point indicated by function `point' is used.

It would call `wlc/fast-in-literal' or `wlc/slow-in-literal'
in implementation."
  (interactive "P")
  (if wlc/fast-delete-flag
      (wlc/fast-in-literal position)
    (wlc/slow-in-literal position)))


(defun wlc/electric-backspace (arg)
  "Delete the preceding character or whitespace.
If `wlc/hungry-delete-key' is non-nil (indicated by \"/h\" on the mode
line) then all preceding whitespace is consumed.  If however a prefix
argument ARG is supplied, or `wlc/hungry-delete-key' is nil
then the function in the variable `wlc/backspace-function' is called."
  (interactive "*P")
  (if (or (not wlc/hungry-delete-key)
          arg
          (wlc/in-literal))
      (funcall wlc/backspace-function (prefix-numeric-value arg))
    (wlc/hungry-delete-backwards)))


(defun wlc/hungry-delete-backwards ()
  "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also `wlc/hungry-delete-forward'."
  (interactive "*")
  (let ((here (point)))
    (c-skip-ws-backward)
    (if (/= (point) here)
        (delete-region (point) here)
      (funcall wlc/backspace-function 1))))

(defalias 'wlc/hungry-backspace 'wlc/hungry-delete-backwards)


(defun wlc/electric-delete-forward (arg)
  "Delete the following character or whitespace.
If `wlc/hungry-delete-key' is non-nil (indicated by \"/h\" on the mode
line) then all following whitespace is consumed.  If however a prefix
argument ARG is supplied, or `wlc/hungry-delete-key' is nil
then the function in the variable `wlc/delete-function' is called."
  (interactive "*P")
  (if (or (not wlc/hungry-delete-key)
          arg
          (wlc/in-literal))
      (funcall wlc/delete-function (prefix-numeric-value arg))
    (wlc/hungry-delete-forward)))


(defun wlc/hungry-delete-forward ()
  "Delete the following character or all following whitespace
up to the next non-whitespace character.
See also `wlc/hungry-delete-backwards'."
  (interactive "*")
  (let ((here (point)))
    (c-skip-ws-forward)
    (if (/= (point) here)
        (delete-region (point) here)
      (funcall wlc/delete-function 1))))


(defun wlc/beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun in `wlc' mode.
To customize the action of `beginning-of-defun' in `wlc' by setting it to
`beginning-of-defun-function', which is called in `beginning-of-defun-raw'.

This function takes the same argument as `beginning-of-defun' and behave
similarly. See `beginning-of-defun' for more information."
  (interactive "^p")
  (let ((pos nil))
    (and (< arg 0) (not (eobp)) (forward-char 1))
    (and (re-search-backward wlc/defun-regexp nil t arg)
         (goto-char (1- (match-end 0)))
         t)))


(defun wlc/indent-context ()
  "Indent code context as much as possible and as smartly as possible."
  (interactive "*")
  (let ((position (point))
        (exit nil))
    (save-excursion
      (if ;; inside a defun
          (and
           (beginning-of-defun) ;; search the beginning of defun using regexp
           (not (wlc/in-literal)) ;; to ensume not in string and comment
           (save-excursion
             ;; TODO try another way to do the sexp forwarding
             ;;      without assert on error
             (forward-sexp)
             (<= position (point))))
          ;; indent the whole defun
          (progn
            (indent-according-to-mode)
            (indent-sexp)
            (setq exit t))))
    ;; not in a defun, indent current line
    (if (not exit)
        (indent-according-to-mode))))


(defun wlc/auto-complete-pair()
  "Automatically complete the right part of pairs used in all lisp modes.
The pairs include `', \"\", [], (), {}."

  ;; make these control variables local to avoid affecting the global settings
  (make-local-variable 'skeleton-pair)
  (make-local-variable 'skeleton-pair-on-word)
  (make-local-variable 'skeleton-pair-filter-function)
  (make-local-variable 'skeleton-pair-alist)

  ;; enable auto pair
  (setq skeleton-pair t)
  ;; don't auto complete pair before or inside a word
  (setq skeleton-pair-on-word nil)
  ;; enable no checking before inserting the complemental pair, which is
  ;; the default value of it. Resetting it here is to prevent it from
  ;; being polluted before this by any global setting, e.g. someone set
  ;; the global value only for c/c++ incorrectly before loading wlc.
  ;; For a comprehensible setup example refer to skeleton.el page 3.
  (setq skeleton-pair-filter-function (lambda () nil))
  ;; customize the complete part on each left part of pairs inserting
  ;; Actually we just keep them the same as default behaviors in skeleton.el.
  (setq skeleton-pair-alist
        '( ;; (?` _ "'") ; we need backquote for macro, no auto-pair
          (?\" _ "\"")
          (?\[ _ "]")
          (?\( _ ")")
          (?{ _ ?})))
  ;; customize which key triggers the auto complete action
  ;;(local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))


;; simple indentation before newline
(defun indent-and-newline ()
  "Indent current line according to major mode and insert a newline.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (indent-according-to-mode)
  (delete-horizontal-space)
  (newline))


(defun wlc/electric-keybindings ()
  "Set key bindings for electric indentation feature."

  (if wlc/electric-flag
      (progn
        (define-key lisp-mode-shared-map [?\r]
          'reindent-then-newline-and-indent)
        (define-key lisp-mode-shared-map [(control c) (i)]
          'wlc/indent-context)
        (define-key lisp-mode-shared-map [(control c) (control i)]
          'wlc/indent-context))
    (progn
      (define-key lisp-mode-shared-map [?\r] 'indent-and-newline)
      (unset-key lisp-mode-shared-map [(control c) (i)])
      (unset-key lisp-mode-shared-map [(control c) (control i)]))))


(defun wlc/keybindings ()
  "Set key bindings for `wlc'."

  ;; Hungry delete feature
  ;; We bind the forward deletion key [delete] and (implicitly) C-d to
  ;; `wlc/electric-delete-forward', and the backward deletion key
  ;; [backspace] to `wlc/electric-backspace'. The hungry variants are
  ;; bound to the same keys but prefixed with C-c. This implies that
  ;; C-c C-d is `wlc/hungry-delete-forward'. For consistency,
  ;; we bind not only C-c <backspace> to `wlc/hungry-delete-backwards'
  ;; but also C-c C-<backspace>, so that the Ctrl key can be held down
  ;; during the whole sequence regardless of the direction.  This in turn
  ;; implies that we bind C-c C-<delete> to `wlc/hungry-delete-forward',
  ;; for the same reason.
  (define-key lisp-mode-shared-map "\C-d" 'wlc/electric-delete-forward)
  (define-key lisp-mode-shared-map "\177" 'wlc/electric-backspace)
  (define-key lisp-mode-shared-map "\C-c\C-d"     'wlc/hungry-delete-forward)
  (define-key lisp-mode-shared-map [?\C-c ?\d]    'wlc/hungry-delete-backwards)
  (define-key lisp-mode-shared-map [?\C-c ?\C-\d] 'wlc/hungry-delete-backwards)
  (define-key lisp-mode-shared-map [?\C-c deletechar] ; C-c <delete> on a tty.
    'wlc/hungry-delete-forward)
  (define-key lisp-mode-shared-map
    [?\C-c (control deletechar)] ; C-c C-<delete> on a tty.
    'wlc/hungry-delete-forward)

  ;; Electric feature
  (wlc/electric-keybindings)

  ;; Define key sequences
  (define-key lisp-mode-shared-map [(control c) (b)] 'backward-up-list)
  (define-key lisp-mode-shared-map [(control c) (control b)] 'backward-up-list)
  (define-key lisp-mode-shared-map [(control c) (l)] 'down-list)
  (define-key lisp-mode-shared-map [(control c) (control l)] 'down-list)
  (define-key lisp-mode-shared-map [(control c) (u)] 'up-list)
  (define-key lisp-mode-shared-map [(control c) (control u)] 'up-list)
  (define-key lisp-mode-shared-map [(control c) (m)] 'mark-sexp)
  (define-key lisp-mode-shared-map [(control c) (control m)] 'mark-sexp)
  (define-key lisp-mode-shared-map [(control c) (k)] 'kill-sexp)
  (define-key lisp-mode-shared-map [(control c) (control k)] 'kill-sexp)
  (define-key lisp-mode-shared-map [(control c) (j)] 'backward-kill-sexp)
  (define-key
    lisp-mode-shared-map [(control c) (control j)] 'backward-kill-sexp)
  (define-key lisp-mode-shared-map [(control c) (e)] 'eval-buffer)
  (define-key lisp-mode-shared-map [(control c) (control e)] 'eval-buffer)

  (define-key lisp-mode-shared-map [(control c) (c)] 'comment-dwim)
  (define-key lisp-mode-shared-map [(control c) (control c)] 'comment-dwim)
  )


(defun wlc/maximum-decoration-on ()
  "Enable maximum decoration (highlighting) in `wlc'."
  ;; add maximum font-lock highlighting for each mode in
  ;; `wlc/maximum-decoration-mode-list'
  (dolist (mode
           wlc/maximum-decoration-mode-list)
    (font-lock-add-keywords mode
                            wlc/font-lock-keywords-elisp 'ADD-TO-END)))


(defun wlc/on ()
  "Turn on wlc extension."
  (interactive)
  (setq beginning-of-defun-function 'wlc/beginning-of-defun)
  ;; set keybings
  (wlc/keybindings)
  ;; show matching paren on point
  (wlc/toggle-show-paren-on-point 1)
  ;; turn on `font-lock-mode' globally
  (global-font-lock-mode t)
  ;; maximum decoration (highlighting)
  (wlc/maximum-decoration-on)

  ;; apply these features to `wlc/all-features-on-mode-hook-list'
  ;; need to add these manually because they are buffer local
  (dolist (mode-hook
           wlc/all-features-on-mode-hook-list)
    (add-hook mode-hook '(lambda ()
                           ;; enable hungry delete feature
                           (wlc/toggle-hungry-state 1)
                           ;; enable automatically complete pair feature
                           (wlc/toggle-auto-complete-pair 1))))
  )


(provide 'wlc)

;;; wlc.el ends here
