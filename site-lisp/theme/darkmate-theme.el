;;; darkmate-theme.el --- Darkmate Color Theme for Emacs

;;; Commentary:
;;
;; Darkmate color theme is a color theme inspired by the darkmate color theme
;; of gEdit, which is inspired by the default color theme of Textmate.

;;; Installation:
;;
;; Add the following codes to your .emacs file:
;;
;; (add-to-list 'custom-theme-load-path directory-of-this-file)
;; (load-theme 'darkmate-theme t)
;;
;; or use `use-package' macro to load the theme:
;;
;; (use-package darkmate-theme
;;   :config
;;   (add-to-list 'custom-theme-load-path directory-of-this-file)
;;   (load-theme 'darkmate-theme t))
;;

;;; Code:

(deftheme darkmate
  "A color theme inspired by the gEdit darkmate color theme.")

(defgroup darkmate-theme nil
  "Darkmate-theme options."
  :group 'faces)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun create-darkmate-theme (name)
  (let* ((class '((class color) (min-colors 256)))
        ;;; color pelette
        ;; white
        (white    (if (true-color-p) "#eeeeee" "color-255"))
        ;; gray -> dark
        (gray     (if (true-color-p) "#bbbbbb" "color-250"))
        (asfalto  (if (true-color-p) "#555753" "color-240"))
        (carbon   (if (true-color-p) "#232323" "color-235"))
        ;; green -> deep
        (senape   (if (true-color-p) "#acc900" "color-148"))
        (lime     (if (true-color-p) "#96ff00" "color-118"))
        (green    (if (true-color-p) "#00c900" "color-40"))
        ;; green -> blue
        (alga     (if (true-color-p) "#00c99b" "color-43"))
        (aque     (if (true-color-p) "#00d8ff" "color-45"))
        (cyan     (if (true-color-p) "#009cff" "color-39"))
        ;; purple
        (violet   (if (true-color-p) "#9e91ff" "color-141"))
        (purple   (if (true-color-p) "#bb66ff" "color-135"))
        ;; pink -> deep red
        (magenta  (if (true-color-p) "#ff79d9" "color-212"))
        (fuschsia (if (true-color-p) "#ff44cc" "color-206"))
        (red      (if (true-color-p) "#ff2f6a" "color-197"))
        ;; yellow
        (yellow   (if (true-color-p) "#fce94f" "color-221"))
        ;; orange -> deep
        (ambra    (if (true-color-p) "#ff9900" "color-208"))
        (orange   (if (true-color-p) "#ff6100" "color-202"))

        ;;;basics
        (link-fg  (if (true-color-p) "#5fafff" "color-75"))
        (mode-line-bg (if (true-color-p) "#1a1a1a" "color-247"))
        (mode-line-fg (if (true-color-p) "#999999" "color-236"))
        (mode-line-buffer-id-bg (if (true-color-p) "#4d4d4d" "color-240"))
        (mode-line-buffer-id-fg (if (true-color-p) cyan "color-39"))
        (mode-line-inactive-bg (if (true-color-p) "#333333" "color-244"))
        (mode-line-inactive-fg (if (true-color-p) "#b3b3b3" "color-237"))

        ;;; linum
        (linum-bg (if (true-color-p) gray "color-250"))
        (linum-fg (if (true-color-p) asfalto "color-240"))
        ;;; linum-relative-current-line-face
        (linum-relative-bg (if (true-color-p) "#e5e5e5" "color-254"))
        (linum-relative-fg (if (true-color-p) asfalto "color-240"))
        )
    (custom-theme-set-faces
     name
;;;;; basics
     ;; text appearance
     `(default ((,class (:background ,carbon :foreground ,white))))
     `(link ((,class (:foreground ,link-fg :underline t))))
     `(link-visited ((,class (:foreground ,link-fg :underline t :slant italic))))
     ;;`(success)
     ;;`(warning)
     ;;`(error)
     ;;`(page-break-lines)
     ;; highlight parts of text temporarily for specific purposes
     `(highlight ((,class (:background "#222222"))))
     `(isearch ((,class (:background ,yellow :foreground ,carbon))))
     `(isearch-fail ((,class (:background ,red :foreground "yellowgreen" :weight bold))))
     `(lazy-highlight ((,class (:background "paleturquoise4"))))
     `(query-replace ((,class (:background ,yellow :foreground ,carbon))))
     `(match ((,class (:background ,white :foreground ,carbon))))
     `(region ((,class (:background ,asfalto))))
     `(secondary-selection ((,class (:background ,gray :foreground ,white))))
     `(trailing-whitespace ((,class (:background "white"))))
     `(escape-glyph ((,class (:foreground "cyan"))))
     ;;`(homoglyph)
     `(nobreak-space ((,class (:foreground "cyan" :underline t))))
     ;;`(nobreak-hyphen)
     ;; appearance of parts of the Emacs frame

     ;; mode-line
     `(mode-line ((,class (:background ,mode-line-bg :foreground ,mode-line-fg :weight semi-bold :box (:line-width -1 :color "#7f7f7f")))))
     `(mode-line-inactive ((,class (:background ,mode-line-inactive-bg :foreground ,mode-line-inactive-fg :box (:line-width -1 :color "#666666")))))
     `(mode-line-buffer-id ((,class (:background ,mode-line-buffer-id-bg :foreground ,mode-line-buffer-id-fg :weight bold))))
     `(mode-line-emphasis ((,class (:weight bold))))
     `(mode-line-highlight ((,class (:inverse-video t))))

     `(header-line ((,class (:background "#333333" :foreground "#e5e5e5" :weight semi-bold))))
     ;; `(header-line-highlight)
     ;; `(tab-line)
     ;;`(border) ;; not included because it's not listed in standard faces docs
     `(vertical-border ((,class (:background "black"))))
     `(minibuffer-prompt ((,class (:foreground ,yellow :weight semi-bold))))
     `(fringe ((,class (:background "#1a1a1a"))))
     `(cursor ((,class (:background ,white))))
     ;; `(tooltip)
     ;; `(mouse)
     `(shadow ((,class (:foreground "#b3b3b3"))))
     ;; control the appearance of parts of the Emacs frame, but only on
     ;; text terminals, or when Emacs is built on X with no toolkit support
     ;; `(scroll-bar)
     ;; `(toll-bar)
     ;; `(tab-bar)
     ;; `(menu)
     ;; `(tty-menu-enabled-face)
     ;; `(tty-menu-disabled-face)
     ;; `(tty-menu-selected-face)

;;;;; font-lock
     `(font-lock-builtin-face ((,class (:foreground ,yellow))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,purple :slant italic))))
     `(font-lock-comment-face  ((,class (:foreground ,purple :slant italic :weight bold))))
     `(font-lock-constant-face ((,class (:foreground ,fuschsia))))
     `(font-lock-doc-face ((,class (:foreground ,lime))))
     `(font-lock-function-name-face ((,class (:foreground ,violet :weight book))))
     `(font-lock-keyword-face ((,class (:foreground ,ambra :weight bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,fuschsia))))
     `(font-lock-preprocessor-face ((,class (:foreground ,aque :weight bold))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,gray))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,fuschsia))))
     `(font-lock-string-face ((,class (:foreground ,lime))))
     `(font-lock-type-face ((,class (:foreground ,cyan))))
     `(font-lock-variable-name-face ((,class (:foreground ,green))))
     `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))

;;;;;; base
;;;;; minibuffer
     ;; `(completions-annotations)
     ;; `(completions-common-part)
     ;; `(completions-first-difference)

;;;;; ido
     `(ido-first-match ((t (:weight bold))))
     `(ido-only-match ((,class (:foreground ,green))))
     `(ido-subdir ((,class (:foreground ,violet :weight bold))))
     ;;`(ido-virtual)
     ;;`(ido-indicator)
     ;;`(ido-incomplete-regexp)

;;;;; helm
;;;;; helm-core
     `(helm-source-header ((,class (:foreground ,purple :weight bold))))
     ;;`(helm-visible-mark)
     ;; helm-header
     ;; helm-candidate-number
     ;; helm-candidate-number-suspended
     `(helm-selection ((,class (:background "black"))))
     ;; helm-separator
     ;; helm-action
     ;; helm-prefarg
     ;; helm-match
     ;; helm-header-line-left-margin

;;;;; dired
     `(dired-directory ((,class (:foreground ,violet :weight bold))))
     `(dired-flagged ((,class (:foreground ,magenta :weight bold))))
     `(dired-header ((,class (:foreground ,purple :slant italic :weight bold))))
     `(dired-ignored ((,class (:foreground "color-249"))))
     `(dired-mark ((,class (:foreground ,cyan))))
     `(dired-marked ((,class (:foreground ,cyan :weight bold))))
     `(dired-perm-write ((,class (:foreground ,green :slant italic))))
     `(dired-symlink ((,class (:foreground ,purple))))
     `(dired-warning ((,class (:foreground ,red :weight bold))))

;;;;; dired+
     ;;`(diredp-autofile-name)
     ;;`(diredp-compressed-file-name)
     `(diredp-compressed-file-suffix ((,class (:foreground ,yellow))))
     `(diredp-date-time ((,class (:foreground ,green :slant italic))))
     `(diredp-deletion ((,class (:foreground ,red :weight bold))))
     `(diredp-deletion-file-name ((,class (:foreground ,red :weight bold))))
     `(diredp-dir-heading ((,class (:foreground ,purple :weight bold))))
     `(diredp-dir-name ((,class (:foreground ,violet :weight bold))))
     `(diredp-dir-priv ((,class (:foreground ,violet :weight bold))))
     `(diredp-exec-priv ((,class (:foreground ,yellow :slant italic))))
     `(diredp-executable-tag ((,class (:foreground ,green))))
     `(diredp-file-name ((,class (:foreground ,white))))
     `(diredp-file-suffix ((,class (:forergound ,white))))
     `(diredp-flag-mark ((,class (:foreground ,cyan))))
     `(diredp-flag-mark-line ((,class (:foreground ,cyan))))
     `(diredp-ignored-file-name ((,class (:foreground "color-249"))))
     ;;`(diredp-link-priv)
     ;;`(diredp-mode-line-marked)
     ;;`(diredp-mode-line-flagged)
     ;;`(diredp-no-priv)
     ;;`(diredp-number)
     ;;`(diredp-other-priv)
     ;;`(diredp-rare-priv)
     ;;`(diredp-read-priv)
     `(diredp-symlink ((,class (:foreground ,purple))))
     ;;`(diredp-tagged-autofile-name)
     `(diredp-write-priv ((,class (:foreground ,fuschsia))))

;;;;; dired-lis
     `(dired-lis-mode-line-face ((,class (nil))))

;;;;; popup
     ;; popup-enu-selection-face
     ;; `(popup-face)
     ;; popup-isearch-match
     ;; popup-menu-face
     ;; popup-menu-selection-face
     ;; popup-scroll-bar-background-face
     ;; popup-scroll-bar-foreground-face
     ;; popup-tip-face


;;;;; cus-edit
     ;; `(custom-botton)

;;;;; wid-edit
     ;; `(widget-documentation)
     ;; `(widget-button)
     ;; `(widget-button-pressed)
     ;; `(widget-field)
     ;; `(widget-single-line-field)
     ;; `(widget-inactive)

;;;;;; editing

;;;;; highlight-current-line
     `(highlight-current-line-face ((,class (:background "color-232"))))

;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:background "color-239"))))

;;;;; hi-lock
     ;; `(hi-black-b)
     ;; `(hi-black-hb)
     ;; `(hi-blue-b)
     ;; `(hi-green)
     ;; `(hi-green-b)
     ;; `(hi-pink)
     ;; `(hi-red-b)
     ;; `(hi-yellow)

;;;;; hl-line
     ;; `(hl-line)

;;;;; outline
     ;; `(outline-1)
     ;; `(outline-2)
     ;; `(outline-3)
     ;; `(outline-4)
     ;; `(outline-5)
     ;; `(outline-6)
     ;; `(outline-7)
     ;; `(outline-8)

;;;;; rainbow-delimiters
     `(rainbow-delimiters-unmatched-face ((,class (:background "red" :weight bold :inverse-video t))))
     ;;`(rainbow-delimiters-mismatched-face)
     `(rainbow-delimiters-depth-1-face ((,class (:foreground "gray60"))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground "orchid"))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground "dodger blue"))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground "sky blue"))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground "lime green"))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground "lawn green"))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground "gold"))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground "sandy brown"))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground "tomato"))))

;;;;; smartparens
     ;;`(sp-pair-overlay-face)
     ;;`(sp-wrap-overlay-face)
     ;;`(sp-wrap-overlay-opening-pair)
     ;;`(sp-wrap-overlay-closing-pair)
     ;;`(sp-wrap-tag-overlay-face)
     `(sp-show-pair-match-face ((,class (:background ,purple :foreground ,white :weight bold))))
     `(sp-show-pair-mismatch-face ((,class (:background ,red :foreground ,white :weight bold))))
     ;;`(sp-show-pair-enclosing)

;;;;; whitespace
     `(whitespace-space ((,class (:background "gray22" :foreground ,magenta))))
     `(whitespace-hspace ((,class (:background "gray22" :forgeground ,magenta))))
     `(whitespace-tab ((,class (:background "gray30" :foreground ,magenta))))
     `(whitespace-space-before-tab ((,class (:background "DarkOrange" :foreground ,magenta))))
     `(whitespace-space-after-tab ((,class (:background "gray22" :foreground ,magenta))))
     `(whitespace-newline ((,class (:background "darkgray"))))
     `(whitespace-trailing ((,class (:background "gray30"))))
     `(whitespace-line ((,class (:background "gray22" :foreground ,violet))))
     `(whitespace-indentation ((,class (:background "gray40" :foreground ,magenta))))
     `(whitespace-big-indent ((,class (:background "gray40" :foreground ,magenta))))
     `(whitespace-empty ((,class (:background "gray22" :foreground ,magenta))))

;;;;;; development

;;;;; lisp
;;;;; eldoc
     ;; `(eldoc-highlight-function-argument)

;;;;; makefile-mode
     `(makefile-space ((,class (:background ,magenta))))
     `(makefile-targets ((,class (:foreground ,cyan :weight semi-bold))))
     `(makefile-shell ((,class (:foreground "wheat" :weight book))))
     `(makefile-makepp-perl ((,class (:background "DarkBlue"))))

;;;;; auto-complete
     ;; `(ac-completion-face)
     ;; `(ac-candidate-face)
     ;; `(ac-candidate-mouse-face)
     ;; `(ac-selection-face)
;;;;; auto-complete-config
     ;; `(ac-yasnippet-candidate-face)
     ;; `(ac-yasnippet-selection-face)

;;;;; term
     `(term ((,class (:background ,carbon :foreground ,white))))
     ;; term-color-black
     ;; term-color-blue
     ;; term-color-cyan
     ;; term-color-green
     ;; term-color-magenta
     ;; term-color-red
     ;; term-color-white
     ;; term-color-yellow

;;;;; which-function-mode
     `(which-func ((,class (:background ,white :foreground ,violet :weight book))))

;;;;; yas
     ;; yas--field-debug-face
     ;; yas-field-highlight-face

;;;;; linum
     `(linum ((,class (:background ,linum-bg :foreground ,linum-fg))))
;;;;; linum-relative
     `(linum-relative-current-line-face ((,class (:background ,linum-relative-bg :foreground ,linum-relative-fg :weight semi-bold))))

;;;;; tools

;;;;; calendar
     `(calender-today ((,class (:background "black" :foreground ,ambra :underline t))))
     `(diary ((,class (:foreground "yellow1"))))
     `(holiday ((,class (:background "gray30" :foreground ,senape :slant italic))))

;;;;; cal-china-x
     `(cal-china-x-general-holiday-face ((,class (:background "green"))))
     `(cal-china-x-important-holiday-face ((,class (:background "red"))))

     )

    (custom-theme-set-variables
     name

;;;;; frame
     ;;`(frame-background-mode 'dark)

;;;;; ansi-color-names
     `(ansi-color-name-vector [,carbon ,red ,green ,yellow ,aque ,magenta ,cyan ,white])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("TODO"        . ,red)
                               ("NEXT"        . ,red)
                               ("THEM"        . ,aque)
                               ("PROG"        . ,cyan)
                               ("OKAY"        . ,cyan)
                               ("DONT"        . ,red)
                               ("FAIL"        . ,red)
                               ("DONE"        . ,red)
                               ("NOTE"        . ,yellow)
                               ("KLUDGE"      . ,yellow)
                               ("HACK"        . ,yellow)
                               ("TEMP"        . ,yellow)
                               ("FIXME"       . ,red)
                               ("XXX+"        . ,red)
                               ("\\?\\?\\?+"  . ,red)))
     )
    ))

(create-darkmate-theme 'darkmate)

(provide-theme 'darkmate)

(provide 'darkmate-theme)

;; darkmate-theme.el ends here
