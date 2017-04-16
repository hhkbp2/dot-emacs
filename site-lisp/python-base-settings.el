;;; python-base-settings.el --- A few base settings for python
;; -*- Emacs-Lisp -*-

;;; Commentary:

;;; Code:


(defconst dw-python-dev-dir (expand-file-name "~/pro/python")
  "Personal development code directory of python.")

(defconst dw-python-path (expand-file-name
                          "~/local/lib/python2.7/site-packages/")
  "Personal PYTHONPATH for addtional libraries.")

(defun dw-prepare-pypath ()
  (let ((pypath (getenv "PYTHONPATH")))
    (if (or (null pypath) (string= "" pypath))
        (setenv "PYTHONPATH" dw-python-path)
      (if (not (search dw-python-path pypath))
          (setenv "PYTHONPATH" (concat dw-python-path ":" pypath))))))

(when (eq system-type 'darwin)
  ;; prepare PYTHONPATH for mac emacs as app started on dock
  (dw-prepare-pypath))


(provide 'python-base-settings)

;;; python-base-settings.el ends here
