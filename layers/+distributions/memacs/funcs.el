;;; memacs/funcs.el --- defines functions for base

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: base functions spacemacs elisp

;;; Commentary:

;; description

;;; Code:


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
