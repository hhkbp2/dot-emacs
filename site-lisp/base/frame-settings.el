;;; frame-settings.el --- Settings for frame

;;; Commentary:

;;; Code:


(require 'font-settings)


(defun dw-frame-settings ()
  "Settings for new frame."

  ;; disable visible bell (and the noisy warning bell)
  (setq visible-bell nil)
  ;; turn off alarms completely
  (setq ring-bell-function 'ignore)

  ;; hide menu-bar under terminal
  (if (not (display-graphic-p))
      (menu-bar-mode -1))

  ;; hide tool-bar
  (tool-bar-mode -1)

  (when (display-graphic-p)
    ;; hide scroll-bar
    (scroll-bar-mode -1)
    ;; 应用font配置
    (font-settings))
  )


(defun frame-settings ()
  "Settings related to frame."

  ;; run this settings function for this frame.
  (dw-frame-settings)
  ;; hook this settings function to future frame creation.
  (add-hook 'after-make-frame-functions
                '(lambda (new-frame)
                   (select-frame new-frame)
                   (dw-frame-settings)))
  )


(provide 'frame-settings)

;;; frame-settings.el ends here
