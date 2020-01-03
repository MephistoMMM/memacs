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

(defun mp-hacking//user-bufferp (buftuple)
  "Test for hidden buffer."
  (not (string-match "\\`[[:space:]]*\\*" (car buftuple))))

(defun mp-hacking/buffer-switch ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :predicate #'mp-hacking//user-bufferp
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer)
  )

(with-eval-after-load 'ivy
  (ivy-configure 'mp-hacking/buffer-switch
    :display-transformer-fn #'ivy-switch-buffer-transformer)
  ;; TODO add current buffer to tmp var before window changing and transform it after window changing
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
