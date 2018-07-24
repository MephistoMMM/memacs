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
    :init
    (progn
      (add-to-list 'auto-mode-alist `("\\.h\\'" . ,c-c++-default-mode-for-headers))
      (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline))
    :config (require 'compile)))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "D" 'disaster)))
    ))

(defun c-c++/post-init-company-ycmd ()
  (spacemacs|add-company-backends :backends company-ycmd :modes c-mode-common))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "D" 'disaster)))))

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
      (setq rtags-autostart-diagnostics t)
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (rtags-diagnostics)
      ;; key bindings
      (evil-define-key 'normal rtags-mode-map
        (kbd "RET")   'rtags-select-other-window
        (kbd "M-RET") 'rtags-select
        (kbd "q")     'rtags-bury-or-delete)
      ;; TODO check for consistency with gtags key bindings
      ;; see https://github.com/syl20bnr/spacemacs/blob/develop/layers/+tags/gtags/funcs.el#L70
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "g." 'spacemacs/c-c++-tags-find-symbol-at-point
          "g," 'spacemacs/c-c++-tags-find-references-at-point
          "g;" 'spacemacs/c-c++-tags-find-file
          "g/" 'rtags-find-all-references-at-point
          "g[" 'rtags-location-stack-back
          "g]" 'rtags-location-stack-forward
          "g>" 'spacemacs/c-c++-tags-find-symbol
          "g<" 'spacemacs/c-c++-tags-find-references
          "gB" 'rtags-show-rtags-buffer
          "gd" 'rtags-print-dependencies
          "gD" 'rtags-diagnostics
          "ge" 'rtags-reparse-file
          "gE" 'rtags-preprocess-file
          "gF" 'rtags-fixit
          "gG" 'rtags-guess-function-at-point
          "gh" 'rtags-print-class-hierarchy
          "gI" 'spacemacs/c-c++-tags-imenu
          "gL" 'rtags-copy-and-print-current-location
          "gM" 'rtags-symbol-info
          "gO" 'rtags-goto-offset
          "gp" 'rtags-set-current-project
          "gR" 'rtags-rename-symbol
          "gs" 'rtags-print-source-arguments
          "gS" 'rtags-display-summary
          "gT" 'rtags-taglist
          "gv" 'rtags-find-virtuals-at-point
          "gV" 'rtags-print-enum-value-at-point
          "gX" 'rtags-fix-fixit-at-point
          "gY" 'rtags-cycle-through-diagnostics)))))

(defun c-c++/post-init-realgud()
  (dolist (mode c-c++-modes)
    (spacemacs/add-realgud-debugger mode "gdb")))

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
      ;; Customize `lsp-project-whitelist' `lsp-project-blacklist' to disable auto initialization.
      (add-hook 'c-mode-common-hook #'memacs//c-c++-cquery-enable)

      ;; add company-lsp
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))
    ))
