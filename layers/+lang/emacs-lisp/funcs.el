;;; funcs.el --- Emacs Lisp functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun spacemacs/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun spacemacs/nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))


;; edebug

(defun spacemacs/edebug-instrument-defun-on ()
  "Toggle on instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun 'edebugit))

(defun spacemacs/edebug-instrument-defun-off ()
  "Toggle off instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun nil))

(defun spacemacs/elisp-toggle-debug-expr-and-eval-func ()
  "Insert or remove debug expression, evaluate function and save buffer."
  (interactive)
  (let ((trace "(debug)")
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (newline-and-indent))))
  (eval-defun nil)
  (save-buffer))

(defun spacemacs//edebug-mode (&rest args)
  "Additional processing when `edebug-mode' is activated or deactivated."
  (let ((evilified t))
    (if (not edebug-mode)
        ;; disable edebug-mode
        (when evilified (evil-normal-state))
      ;; enable edebug-mode
      (when evilified (evil-evilified-state))
      (evil-normalize-keymaps))))


;; elisp comment text-object definition

(defun spacemacs//define-elisp-comment-text-object ()
  "Define a text object and a surround pair for elisp comments.
Intended for use in mode hooks."
  (spacemacs|define-text-object ";" "elisp-comment" ";; " ""))
