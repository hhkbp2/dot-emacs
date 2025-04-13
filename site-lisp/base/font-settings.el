;;; font-settings.el --- Settings for font

;;; Commentary:

;;; Code:


(defconst dw-favor-en-font-list
  '("Monego Nerd Font Fix"
    "JetBrainsMonoNL Nerd Font Mono"
    )
  "Personal favor font list for language en in descendent order.")

(defconst dw-favor-zh-font-list
  '("霞鹜文楷等宽"
    "京華老宋体"
    "终端更纱黑体-简 Nerd"
    )
  "Personal favor font list for language zh in descendent order.")

(setq dw-font-pairs
      (case system-type
        (`darwin
         '(
           (("Monego Nerd Font Fix" . "霞鹜文楷等宽") .
            ;; font-spec parameters, rescale ratio
            (((:size 14) . 1.0) . ((:size 16) . 1.0)))
           (("Monego Nerd Font Fix" . "京華老宋体") .
            ;; font-spec parameters, rescale ratio
            (((:size 14) . 1.0) . ((:size 16) . 1.0)))
           (("Monego Nerd Font Fix" . "终端更纱黑体-简 Nerd") .
            ;; font-spec parameters, rescale ratio
            (((:size 14) . 1.0) . ((:size 16) . 1.0)))
           (("JetBrainsMonoNL Nerd Font Mono" . "霞鹜文楷等宽") .
            ;; font-spec parameters, rescale ratio
            (((:size 14) . 1.0) . ((:size 16) . 1.0)))
           (("JetBrainsMonoNL Nerd Font Mono" . "终端更纱黑体-简 Nerd") .
            ;; font-spec parameters, rescale ratio
            (((:size 14) . 1.0) . ((:size 16) . 1.0)))
           ))
        (`gnu/linux
         '(
           (("Monego Nerd Font Fix" . "霞鹜文楷等宽") .
            ;; font-spec parameters, rescale ratio
            (((:size 18) . 1.0) . ((:size 22) . 1.0)))
           (("Monego Nerd Font Fix" . "京華老宋体") .
            ;; font-spec parameters, rescale ratio
            (((:size 18) . 1.0) . ((:size 20) . 1.0)))
           (("Monego Nerd Font Fix" . "终端更纱黑体-简 Nerd") .
            ;; font-spec parameters, rescale ratio
            (((:size 18) . 1.0) . ((:size 22) . 1.0)))
           (("JetBrainsMonoNL Nerd Font Mono" . "霞鹜文楷等宽") .
            ;; font-spec parameters, rescale ratio
            (((:size 18) . 1.0) . ((:size 22) . 1.0)))
           (("JetBrainsMonoNL Nerd Font Mono" . "终端更纱黑体-简 Nerd") .
            ;; font-spec parameters, rescale ratio
            (((:size 18) . 1.0) . ((:size 22) . 1.0)))
           ))
        )
      )


(defun dw-font-exsits-p (font-name-pattern)
  "Return t if any available font matches `font-name-pattern', else nil."
  (x-list-fonts font-name-pattern))

(defun dw-first-available-font (font-list)
  "Return first available font in `font-list'."
  (find-if #'dw-font-exsits-p font-list))

(defun dw-font-pair-exists-p (pair)
  (assoc pair dw-font-pairs))

(defun dw-available-font-pair (en-font-list zh-font-list)
  (let* ((en-font-name (dw-first-available-font en-font-list))
         (zh-font-name (dw-first-available-font zh-font-list))
         (font-pair (cons en-font-name zh-font-name)))
    (unless (and en-font-name zh-font-name)
      (error "no available font in font list en: %s and zh: %s"
             en-font-list zh-font-list))
    (unless (dw-font-pair-exists-p font-pair)
      (error "no available font pair for font en: %s and zh: %s"
             en-font-name zh-font-name))
    font-pair))

(defun dw-font-spec-for-pair (pair)
  (let ((pair-parameters (cdr (assoc pair dw-font-pairs))))
    (cl-flet ((get-font-spec (name paras)
                             (apply #'font-spec :family name paras)))
      (cl-values (get-font-spec (car pair) (car (car pair-parameters)))
                 (get-font-spec (cdr pair) (car (cdr pair-parameters)))))))

(defun dw-font-rescale-alist-for-pair (pair)
  (let ((pair-parameters (cdr (assoc pair dw-font-pairs))))
    (cl-flet ((get-regexp (font-name)
                          (replace-regexp-in-string "\\s-" "." font-name)))
      (list  (cons (get-regexp (car pair)) (cdr (car pair-parameters)))
             (cons (get-regexp (cdr pair)) (cdr (cdr pair-parameters)))))))


(defun dw-set-font (en-font-list zh-font-list)
  "Set font for language en and zh.
`en-font-list' and `zh-font-list' should be a list of font name string.
It will try to find out the existing font in each list to perform a proper
settings according to `dw-font-pairs'."

  (let* ((font-pair (dw-available-font-pair en-font-list zh-font-list)))
    ;; simulate to multiple return values
    (cl-multiple-value-bind
        (en-font-spec zh-font-spec) (dw-font-spec-for-pair font-pair)
      ;; (set-face-attribute 'default t
      ;;                     :font en-font-spec  :weight 'normal :width 'normal
      ;; set en font
      (set-frame-font en-font-spec)
      ;; set zh font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          zh-font-spec)))))


(defadvice frame-notice-user-settings (before dw-rescale-alist)
  (when (display-graphic-p)
    (let ((font-pair (dw-available-font-pair
                      dw-favor-en-font-list dw-favor-zh-font-list)))
      (dolist (elem (dw-font-rescale-alist-for-pair font-pair))
        (add-to-list 'face-font-rescale-alist
                     elem)))))
(ad-activate 'frame-notice-user-settings)


(defun font-settings ()
  "Settings for font."

  (when (display-graphic-p)
    (setq-default line-spacing nil)
    (dw-set-font dw-favor-en-font-list dw-favor-zh-font-list)))


(provide 'font-settings)

;;; font-settings.el ends here
