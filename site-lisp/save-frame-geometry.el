;; -*- Emacs-Lisp -*-
;; Save frame's geometry on exit and reconfig it on startup.

;; A load from
;; http://ck.kennt-wayne.de/2011/jul/emacs-restore-last-frame-size-on-startup

;;; Code:

(defun save-frame-geometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/frame-geometry."
  (let (
        (frame-geometry-left (frame-parameter (selected-frame) 'left))
        (frame-geometry-top (frame-parameter (selected-frame) 'top))
        (frame-geometry-width (frame-parameter (selected-frame) 'width))
        (frame-geometry-height (frame-parameter (selected-frame) 'height))
        (frame-geometry-file (expand-file-name "~/.emacs.d/frame-geometry"))
        )

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max frame-geometry-top 0))
       (format "        (left . %d)\n" (max frame-geometry-left 0))
       (format "        (width . %d)\n" (max frame-geometry-width 0))
       (format "        (height . %d)))\n" (max frame-geometry-height 0)))
      (when (file-writable-p frame-geometry-file)
        (write-file frame-geometry-file))))
  )

(defun load-frame-geometry ()
  "Loads ~/.emacs.d/frame-geometry which should load the previous frame's geometry."
  (let ((frame-geometry-file (expand-file-name "~/.emacs.d/frame-geometry")))
    (when (file-readable-p frame-geometry-file)
      (load-file frame-geometry-file)))
  )

(defun save-frame-geometry-on
    ;; Special work to do ONLY when there is a window system being used
    (if window-system
        (progn
          (add-hook 'after-init-hook 'load-frame-geometry)
          (add-hook 'kill-emacs-hook 'save-frame-geometry))
      )
  )

(provide 'save-frame-geometry)

