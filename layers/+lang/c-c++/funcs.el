;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))


;; realgud

(defun spacemacs//short-key-state (modeon)
  "Set evil-evilified-state explicitly."
  (if modeon
      (evil-evilified-state)
    (evil-normal-state)))


;; lsp
(defun memacs//c-c++-cquery-enable ()
  (if (and buffer-file-name
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
      ;; t
      (lsp-cquery-enable)
    ;; nil
    (message "Not find .cquery or compile_commands, cquery is disable.")
    ))
