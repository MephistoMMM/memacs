;;; memacs/funcs.el --- defines functions for base

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: base functions spacemacs elisp

;;; Commentary:

;; description

;;; Code:


;;;; Spaceline

(defun memacs/spaceline-compile ()
  "Spaceline Compile Using Myself Theme"
  (spaceline-compile
    memacs-spaceline-left-segments
    memacs-spaceline-right-segments)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(defun spacemacs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "spacemacs-%S-face" state))))

(defun spacemacs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs//restore-buffers-powerline ()
  "Restore the powerline in all buffers."
  (dolist (buffer (buffer-list))
    (spacemacs//restore-powerline buffer)))

(defun spacemacs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm spacemacs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))


;;;; Recompile

(defun memacs//recompile-dir (dir-alias)
  (byte-recompile-directory (concat spacemacs-start-directory dir-alias) 0))

(defun memacs//recompile-file (file-alias)
  (byte-compile-file (concat spacemacs-start-directory file-alias) 0))

(defun memacs/recompile ()
  "Recompile emacs-lisp files in ~/.emacs.d"
  (interactive)
  (dolist (e memacs-recompile-list)
    (if (string= (file-name-extension e) "el")
        (memacs//recompile-file e)
      (memacs//recompile-dir e))
    )
  (byte-compile-file dotspacemacs-filepath 0)
  )

;;; memacs/funcs.el ends here
