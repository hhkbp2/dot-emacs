;;; gnome-adwaita-theme.el --- Adwaita Color Theme for Emacs

;;; Commentary:
;;
;; The gnome adwaita color theme is a color theme inspired by the
;; color palette of Gnome theme adwaita.
;;

;;; Installation:
;;
;; Add the following codes to your .emacs file:
;;
;; (add-to-list 'custom-theme-load-path directory-of-this-file)
;; (load-theme 'gnome-adwaita t)
;;
;; or use `use-package' macro to load the theme:
;;
;; (use-package gnome-adwaita
;;   :config
;;   (add-to-list 'custom-theme-load-path directory-of-this-file)
;;   (load-theme 'gnome-adwaita t))
;;

;;; Code:


(deftheme gnome-adwaita
  "A color theme inspired by the Gnome Adwaita color palette.")

(defgroup gnome-adwaita-theme nil
  "Gnome-adwaita-theme options."
  :group 'faces)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun create-adwaita-theme (name)
  (let* ((class '((class color) (min-colors 256)))
         ;;; basics
         ;; black and white
         (absolute-black (if (true-color-p) "#000000" "color-0"))
         (black          (if (true-color-p) "#2e3436" "color-236"))
         (absolute-white (if (true-color-p) "#ffffff" "color-231"))
         (white          (if (true-color-p) "#ededed" "color-255"))
         ;; The gray colors in descending brightness order:
         (light-gray     (if (true-color-p) "#ddd9d6" "color-188"))
         (mid-gray       (if (true-color-p) "#cccccc" "color-251"))
         (gray           (if (true-color-p) "#b2b2b2" "color-249"))
         (deep-gray      (if (true-color-p) "#8c8c8c" "color-245"))
         ;; Notice about the grayscale and setup, the following setups are used
         ;; in different sections:
         ;; default section:     bg white,      fg black
         ;; light gray section:  bg light-gray, fg black
         ;; mid-gray section:    bg mid-gray,   fg black
         ;; deeper gray section: bg gray,       fg black
         ;; while deep-gray is so dark that it's only used for decoration

         ;; purple
         (violet         (if (true-color-p) "#9435e4" "color-98"))
         (purple         (if (true-color-p) "#6102b1" "color-55"))

         ;; blue
         (sky-blue       (if (true-color-p) "#6799cc" "color-68"))
         (steel-blue     (if (true-color-p) "#00bbff" "color-39"))
         (blue           (if (true-color-p) "#3584e4" "color-32"))
         (dodger-blue    (if (true-color-p) "#0066cc" "color-26"))
         (deep-blue      (if (true-color-p) "#00578e" "color-24"))

         ;; green
         (green          (if (true-color-p) "#35e43d" "color-77"))
         (olive-green    (if (true-color-p) "#9cbb43" "color-143"))
         (sea-green      (if (true-color-p) "#4cb64a" "color-71"))
         (chartreuse     (if (true-color-p) "#4e9a06" "color-64"))
         (dark-green     (if (true-color-p) "#2f8b58" "color-29"))

         ;; yellow
         (wheat          (if (true-color-p) "#feffbf" "color-229"))
         (yellow         (if (true-color-p) "#dce435" "color-185"))
         (gold           (if (true-color-p) "#f8d00e" "color-220"))
         (orange         (if (true-color-p) "#ce5c00" "color-166"))

         ;; red
         (pink           (if (true-color-p) "#ff7092" "color-204"))
         (hot-pink       (if (true-color-p) "#f74d97" "color-206"))
         (indian-red     (if (true-color-p) "#e43d35" "color-167"))
         (wine-red       (if (true-color-p) "#b50000" "color-160"))
         (dark-red       (if (true-color-p) "#a52a2a" "color-88"))

         ;; basics
         (fg black)
         (bg white)

         ;; ansi color palette
         (ansi-black        deep-gray)
         (ansi-red          indian-red)
         (ansi-green        green)
         (ansi-yellow       yellow)
         (ansi-blue         blue)
         (ansi-magenta      (if (true-color-p) "#e435dc" "color-170"))
         (ansi-cyan         (if (true-color-p) "#35dce4" "color-80"))
         (ansi-white        white)
         )

    (custom-theme-set-faces
     name

;;;;; basics
     ;; text appearance
     `(default ((,class (:background ,bg :foreground ,fg))));
     `(link ((,class (:foreground ,dodger-blue :underline t))));
     `(link-visited ((,class (:foreground ,sky-blue :underline t :slant italic))));
     `(success ((,class (:foreground ,chartreuse :bold t))))
     `(warning ((,class (:foreground ,orange))))
     `(error ((,class (:foreground ,wine-red))))
     ;;`(page-break-lines)
     ;; highlight parts of text temporarily for specific purposes
     `(highlight ((,class (:background ,light-gray))))
     `(isearch ((,class (:background ,chartreuse :foreground ,white))))
     `(isearch-fail ((,class (:background ,wine-red :foreground ,white :weight bold))))
     `(lazy-highlight ((,class (:background ,sky-blue :foreground ,white))))
     `(query-replace ((,class (:background ,chartreuse :foreground ,white))))
     `(match ((,class (:background ,chartreuse :foreground ,white))))
     `(region ((,class (:background ,steel-blue :foreground ,white))))
     ;; `(secondary-selection ((,class (:background ,gray :foreground ,white))))
     `(trailing-whitespace ((,class (:background ,fg))))
     ;; `(escape-glyph ((,class (:foreground "cyan"))))
     ;;`(homoglyph)
     ;;`(nobreak-space ((,class (:foreground "cyan" :underline t))))
     ;;`(nobreak-hyphen)
     ;; appearance of parts of the Emacs frame

     ;; mode-line
     `(mode-line ((,class (:background ,white :foreground ,black :weight semi-bold :box (:line-width -1 :style released-button)))))
     `(mode-line-inactive ((,class (:background ,absolute-white :foreground ,mid-gray))))
     `(mode-line-buffer-id ((,class (:background ,mid-gray :foreground ,black :weight bold))))

     `(mode-line-emphasis ((,class (:weight bold))))
     `(mode-line-highlight ((,class (:inverse-video t))))

     ;; `(header-line ((,class (:background "#333333" :foreground "#e5e5e5" :weight semi-bold))))
     ;; `(header-line-highlight)
     ;; `(tab-line)
     ;;`(border) ;; not included because it's not listed in standard faces docs
     `(vertical-border ((,class (:background ,mid-gray))))
     `(minibuffer-prompt ((,class (:foreground ,blue :weight semi-bold))))
     `(fringe ((,class (:background ,mid-gray))))
     `(cursor ((,class (:background ,blue))))
     ;; `(tooltip)
     ;; `(mouse)
     `(shadow ((,class (:foreground ,gray))))
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
     `(font-lock-builtin-face ((,class (:foreground ,violet))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,deep-blue))))
     `(font-lock-comment-face  ((,class (:foreground ,deep-blue :slant italic))))
     `(font-lock-constant-face ((,class (:foreground ,hot-pink))))
     `(font-lock-doc-face ((,class (:foreground ,chartreuse))))
     `(font-lock-function-name-face ((,class (:foreground ,dodger-blue :bold t))))
     `(font-lock-keyword-face ((,class (:foreground ,dark-red :weight bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,hot-pink))))
     `(font-lock-preprocessor-face ((,class (:foreground ,indian-red))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,indian-red))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,hot-pink))))
     `(font-lock-string-face ((,class (:foreground ,chartreuse))))
     `(font-lock-type-face ((,class (:foreground ,dark-green :bold t))))
     `(font-lock-variable-name-face ((,class (:foreground ,sea-green :bold t))))
     `(font-lock-warning-face ((,class (:foreground ,orange :bold t))))

;;;;;; base
;;;;; minibuffer
     ;; `(completions-annotations)
     ;; `(completions-common-part)
     ;; `(completions-first-difference)

;;;;; ido
     `(ido-first-match ((t (:foreground ,sea-green))))
     `(ido-only-match ((,class (:foreground ,chartreuse :weight bold))))
     `(ido-subdir ((,class (:foreground ,dodger-blue :weight bold))))
     ;;`(ido-virtual)
     ;;`(ido-indicator)
     ;;`(ido-incomplete-regexp)

;;;;; helm
;;;;; helm-core
     ;; `(helm-source-header ((,class (:foreground ,purple :weight bold))))
     ;;`(helm-visible-mark)
     ;; helm-header
     ;; helm-candidate-number
     ;; helm-candidate-number-suspended
     ;; `(helm-selection ((,class (:background "black"))))
     ;; helm-separator
     ;; helm-action
     ;; helm-prefarg
     ;; helm-match
     ;; helm-header-line-left-margin

;;;;; dired
     ;; `(dired-directory ((,class (:foreground ,dodger-blue :weight bold))))
     ;; `(dired-flagged ((,class (:foreground ,magenta :weight bold))))
     ;; `(dired-header ((,class (:foreground ,dodger-blue :slant italic :weight bold))))
     ;; `(dired-ignored ((,class (:foreground "color-249"))))
     ;; `(dired-mark ((,class (:foreground ,cyan))))
     ;; `(dired-marked ((,class (:foreground ,cyan :weight bold))))
     ;; `(dired-perm-write ((,class (:foreground ,green :slant italic))))
     ;; `(dired-symlink ((,class (:foreground ,purple))))
     ;; `(dired-warning ((,class (:foreground ,red :weight bold))))

;;;;; dired+
     ;;`(diredp-autofile-name)
     ;;`(diredp-compressed-file-name)
     ;; `(diredp-compressed-file-suffix ((,class (:foreground ,yellow))))
     ;; `(diredp-date-time ((,class (:foreground ,green :slant italic))))
     `(diredp-deletion ((,class (:foreground ,indian-red :weight bold))))
     `(diredp-deletion-file-name ((,class (:foreground ,indian-red :weight bold))))
     `(diredp-dir-heading ((,class (:foreground ,dark-green :weight bold))))
     `(diredp-dir-name ((,class (:foreground ,dodger-blue :weight bold))))
     ;; `(diredp-dir-priv ((,class (:foreground ,violet :weight bold))))
     ;; `(diredp-exec-priv ((,class (:foreground ,yellow :slant italic))))
     ;; `(diredp-executable-tag ((,class (:foreground ,green))))
     `(diredp-file-name ((,class (:foreground ,fg))))
     `(diredp-file-suffix ((,class (:forergound ,fg))))
     `(diredp-flag-mark ((,class (:foreground ,ansi-cyan))))
     ;; `(diredp-flag-mark-line ((,class (:foreground ,cyan))))
     ;; `(diredp-ignored-file-name ((,class (:foreground "color-249"))))
     ;;`(diredp-link-priv)
     ;;`(diredp-mode-line-marked)
     ;;`(diredp-mode-line-flagged)
     ;;`(diredp-no-priv)
     ;;`(diredp-number)
     ;;`(diredp-other-priv)
     ;;`(diredp-rare-priv)
     ;;`(diredp-read-priv)
     ;; `(diredp-symlink ((,class (:foreground ,purple))))
     ;;`(diredp-tagged-autofile-name)
     ;; `(diredp-write-priv ((,class (:foreground ,fuschsia))))

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
     `(highlight-current-line-face ((,class (:background ,light-gray))))

;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:background ,sky-blue))))

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
     `(rainbow-delimiters-unmatched-face ((,class (:background ,wine-red :foreground ,white :weight bold))))
     ;;`(rainbow-delimiters-mismatched-face)
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,purple))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,deep-blue))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,dodger-blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,dark-green))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,olive-green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,gold))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,pink))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,indian-red))))

;;;;; smartparens
     ;;`(sp-pair-overlay-face)
     ;;`(sp-wrap-overlay-face)
     ;;`(sp-wrap-overlay-opening-pair)
     ;;`(sp-wrap-overlay-closing-pair)
     ;;`(sp-wrap-tag-overlay-face)
     `(sp-show-pair-match-face ((,class (:background ,sky-blue :foreground ,white))))
     `(sp-show-pair-mismatch-face ((,class (:background ,wine-red :foreground ,white :weight bold))))
     ;;`(sp-show-pair-enclosing)

;;;;; whitespace
     ;; `(whitespace-space ((,class (:background "gray22" :foreground ,magenta))))
     ;; `(whitespace-hspace ((,class (:background "gray22" :forgeground ,magenta))))
     ;; `(whitespace-tab ((,class (:background "gray30" :foreground ,magenta))))
     ;; `(whitespace-space-before-tab ((,class (:background "DarkOrange" :foreground ,magenta))))
     ;; `(whitespace-space-after-tab ((,class (:background "gray22" :foreground ,magenta))))
     ;; `(whitespace-newline ((,class (:background "darkgray"))))
     ;; `(whitespace-trailing ((,class (:background "gray30"))))
     ;; `(whitespace-line ((,class (:background "gray22" :foreground ,violet))))
     ;; `(whitespace-indentation ((,class (:background "gray40" :foreground ,magenta))))
     ;; `(whitespace-big-indent ((,class (:background "gray40" :foreground ,magenta))))
     ;; `(whitespace-empty ((,class (:background "gray22" :foreground ,magenta))))

;;;;;; development

;;;;; lisp
;;;;; eldoc
     ;; `(eldoc-highlight-function-argument)

;;;;; makefile-mode
     ;; `(makefile-space ((,class (:background ,magenta))))
     ;; `(makefile-targets ((,class (:foreground ,cyan :weight semi-bold))))
     ;; `(makefile-shell ((,class (:foreground "wheat" :weight book))))
     ;; `(makefile-makepp-perl ((,class (:background "DarkBlue"))))

;;;;; auto-complete
     ;; `(ac-completion-face)
     ;; `(ac-candidate-face)
     ;; `(ac-candidate-mouse-face)
     ;; `(ac-selection-face)
;;;;; auto-complete-config
     ;; `(ac-yasnippet-candidate-face)
     ;; `(ac-yasnippet-selection-face)

;;;;; term
     `(term ((,class (:background ,bg :foreground ,fg))))
     `(term-color-black ((,class (:foreground ,ansi-black))))
     `(term-color-blue ((,class (:foreground ,ansi-blue))))
     `(term-color-cyan ((,class (:foreground ,ansi-cyan))))
     `(term-color-green ((,class (:foreground ,ansi-green))))
     `(term-color-magenta ((,class (:foreground ,ansi-magenta))))
     `(term-color-red ((,class (:foreground ,ansi-red))))
     `(term-color-white ((,class (:foreground ,ansi-white))))
     `(term-color-yellow ((,class (:foreground ,ansi-yellow))))

;;;;; which-function-mode
     ;; `(which-func ((,class (:background ,white :foreground ,violet :weight book))))

;;;;; yas
     ;; yas--field-debug-face
     ;; yas-field-highlight-face

;;;;; linum
     `(linum ((,class (:background ,light-gray :foreground ,deep-gray))))
;;;;; linum-relative
     `(linum-relative-current-line-face ((,class (:background ,light-gray :foreground ,deep-gray))))

;;;;; tools

;;;;; calendar
     `(calender-today ((,class (:background ,blue :foreground ,white :underline t))))
     ;; `(diary ((,class (:foreground "yellow1"))))
     `(holiday ((,class (:background ,indian-red :foreground ,white :slant italic))))

;;;;; cal-china-x
     ;; `(cal-china-x-general-holiday-face ((,class (:background "green"))))
     ;; `(cal-china-x-important-holiday-face ((,class (:background "red"))))

;;;;; extras

     )

    (custom-theme-set-variables
     name

;;;;; frame
     ;;`(frame-background-mode 'light)

;;;;; ansi-color-names
     `(ansi-color-name-vector [,ansi-black
                               ,ansi-red
                               ,ansi-green
                               ,ansi-yellow
                               ,ansi-blue
                               ,ansi-magenta
                               ,ansi-cyan
                               ,ansi-white])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("TODO"        . ,wine-red)
                               ("NEXT"        . ,wine-red)
                               ("THEM"        . ,blue)
                               ("PROG"        . ,ansi-cyan)
                               ("OKAY"        . ,ansi-cyan)
                               ("DONT"        . ,wine-red)
                               ("FAIL"        . ,wine-red)
                               ("DONE"        . ,wine-red)
                               ("NOTE"        . ,gold)
                               ("KLUDGE"      . ,gold)
                               ("HACK"        . ,gold)
                               ("TEMP"        . ,gold)
                               ("FIXME"       . ,wine-red)
                               ("XXX+"        . ,wine-red)
                               ("\\?\\?\\?+"  . ,wine-red)))
     )
    ))

(create-adwaita-theme 'gnome-adwaita)

(provide-theme 'gnome-adwaita)

(provide 'gnome-adwaita-theme)

;;; gnome3-adwaita-theme.el ends here
