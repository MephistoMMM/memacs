;;; funcs.el --- Define functions for mp

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp mp-hacking

;;; Commentary:

;;

;;; Code:


;;;; common

(defun mp-hacking/insert-form-feed ()
  "Insert a FORM FEED(^L)"
  (interactive)
  (beginning-of-line)
  (insert "\n")
  )


;;;; haskell

(defun mp-hacking/format-haskell-buffer ()
  "format haskell buffer by hindent-reformat-buffer and haskell-mode-stylish-buffer"
  (interactive)
  (hindent-reformat-buffer)
  (haskell-mode-stylish-buffer))

(defun mp-hacking/hungry-delete ()
  "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character."
  (interactive)
  (unless (or (not (char-before)) (> (char-before) 32))
    (let ((limit (point-min))
          (here (point)))
      (while (progn
               (skip-chars-backward " \t\n\r\f\v" limit)
               (and (eolp)
                    (eq (char-before) ?\\)))
        (backward-char))
      (if (/= (point) here)
          (delete-region (point) here)))
    )
  )


;;;; outline-ivy

(defun mp-hacking//advise-outshine-narrow-start-pos ()
  (unless (outline-on-heading-p t)
    (outline-previous-visible-heading 1)))

;;; funcs.el ends here
