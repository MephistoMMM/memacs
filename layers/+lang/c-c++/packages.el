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
    srefactor
    stickyfunc-enhance

    ;; lsp
    (ccls :requires lsp-mode company-lsp)
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

;; See also https://github.com/MaskRay/ccls/wiki/Emacs
(defun c-c++/init-ccls ()
  (use-package ccls
    :commands lsp-ccls-enable
    :defer t
    :init
    (progn
      (setq ccls-executable "/usr/local/bin/ccls")
      ;; Customize `lsp-project-whitelist' `lsp-project-blacklist' to disable auto initialization.
      (add-hook 'c-mode-common-hook #'memacs//c-c++-ccls-enable)

      (with-eval-after-load 'projectile
        (setq projectile-project-root-files-top-down-recurring
              (append '("compile_commands.json"
                        ".ccls")
                      projectile-project-root-files-top-down-recurring))
        (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

      ;; add company-lsp
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))
    :config
    (progn
      (setq ccls-sem-highlight-method 'font-lock)
      (ccls-use-default-rainbow-sem-highlight))
    ))
