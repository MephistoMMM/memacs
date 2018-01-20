;;; funcs.el --- Define functions for mp

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp mp-hacking

;;; Commentary:

;;

;;; Code:

(defun mp-hacking/better-default ()
  "This function set many default config to more better!
Some configurations should be written at here which be unable to config
in 'with-eval-after-load.")

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

(defun mp-hacking/hungry-delete-current-line ()
  "Delete all spaces before current point in current line"
  (interactive)
  (let ((here (point)))
    (while (progn
             (skip-syntax-backward " \t\r\f\v")
             (eq (char-before) ?\\))
      (backward-char))
    (if (< (point) here)
        (delete-region (point) here))
   )
 )

(defun mp-hacking/hide-js2-checks-if-flycheck-active ()
  "This function hide the result of js2-mode checking when there is a checker for flycheck."
  (message "fuck")
  (if (flycheck-get-checker-for-buffer)
      ;;t
      (js2-mode-hide-warnings-and-errors)
    ;;nil
    (js2-mode-display-warnings-and-errors)))

;;; funcs.el ends here
