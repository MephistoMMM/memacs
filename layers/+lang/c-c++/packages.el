;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-c++-packages
  '(
    cc-mode
    disaster
    flycheck
    gdb-mi
    google-c-style
    realgud
    srefactor
    stickyfunc-enhance
    xcscope

    ;; lsp
    (cquery :requires lsp-mode company-lsp)
    ))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,c-c++-default-mode-for-headers))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1))))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode)))

(defun c-c++/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++/init-realgud()
  (use-package realgud
    :defer t
    :commands (realgud:gdb)
    :init
    (progn
      (advice-add 'realgud-short-key-mode-setup
                  :before #'spacemacs//short-key-state)
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "s" 'realgud:cmd-next
        "i" 'realgud:cmd-step
        "b" 'realgud:cmd-break
        "B" 'realgud:cmd-clear
        "o" 'realgud:cmd-finish
        "c" 'realgud:cmd-continue
        "e" 'realgud:cmd-eval
        "r" 'realgud:cmd-restart
        "q" 'realgud:cmd-quit
        "S" 'realgud-window-cmd-undisturb-src))))

(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :defer
    :if (or 'c-c++-enable-google-style 'c-c++-enable-google-newline)
    :init (progn
            (when c-c++-enable-google-style (add-hook 'c-mode-common-hook 'google-set-c-style))
            (when c-c++-enable-google-newline (add-hook 'c-mode-common-hook 'google-make-newline-indent)))))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r" 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++/pre-init-xcscope ())

;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun c-c++/init-cquery ()
  (use-package cquery
    :defer t
    :init
    (progn
      (setq cquery-executable "/usr/local/bin/cquery")
      (setq cquery-extra-args '("--language-server" "--log-file=/tmp/cq.log"))
      ;; Customize `lsp-project-whitelist' `lsp-project-blacklist' to disable auto initialization.
      (add-hook 'c-mode-common-hook #'memacs//c-c++-cquery-enable)

      ;; add company-lsp
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))
    ))
