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

(defun mp-hacking//user-bufferp (bufname)
  "Test for hidden buffer."
  (not (string-match "\\`[[:space:]]*\\*" bufname)))

(defun mp-hacking/buffer-switch ()
  (interactive)
  ;; all the buffers
  (setq full-buffer-list (seq-filter 'mp-hacking//user-bufferp
                                     (mapcar (function buffer-name) (buffer-list))))
  (if mp-hacking-buffer-switch-max
      (progn
        ;; if max specified, only take n buffers
        (setq buffer-select-list (subseq full-buffer-list
                                         1
                                         (min
                                          (length full-buffer-list)
                                          (+ mp-hacking-buffer-switch-max 1))))
        )
    ;; if not specified, take all
    (setq buffer-select-list full-buffer-list)
    )
  (if (> (length buffer-select-list) 0)
    (progn
      (setq dest-buffer (popup-menu* buffer-select-list :keymap switch-keymap))
      (switch-to-buffer dest-buffer))
    (message "Non Other User's Buffer.")
    )
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
