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
        google-c-style
        stickyfunc-enhance

        ;; normal
        clang-format
        (company-c-headers :requires company)
        counsel-gtags
        ggtags

        ;; lsp
        (cquery :requires lsp-mode)
        projectile

        (cpp-auto-include
         :location (recipe :fetcher github
                           :repo "syohex/emacs-cpp-auto-include"))
        eldoc
        org
        realgud
        srefactor
        ;; lsp
        (ccls :requires lsp-mode)
        dap-mode
        ;; ycmd
        (company-ycmd :requires company)
        (flycheck-ycmd :requires flycheck)
        ycmd))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-backend)
      (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-backend)
      (put 'c-c++-backend 'safe-local-variable 'symbolp)
      (when c-c++-default-mode-for-headers
        (add-to-list 'auto-mode-alist
                     `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
      (when c-c++-enable-auto-newline
        (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline)))
    :config (require 'compile)))

(defun c-c++/init-ccls ()
  (use-package ccls
    :defer t))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :init (spacemacs//c-c++-setup-clang-format)))

(defun c-c++/post-init-company ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-company)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-company))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :commands company-ycmd))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/init-cpp-auto-include ()
  (use-package cpp-auto-include
    :defer t
    :init
    (progn
      (when c++-enable-organize-includes-on-save
        (add-hook 'c++-mode-hook #'spacemacs/c++-organize-includes-on-save))

      (spacemacs/declare-prefix-for-mode 'c++-mode
        "mr" "refactor")
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ri" #'spacemacs/c++-organize-includes))))

(defun c-c++/init-cquery ()
  (use-package cquery
    :defer t))

(defun c-c++/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'c-mode)
  (add-to-list 'spacemacs--dap-supported-modes 'c++-mode)
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-dap)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-dap))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)))

(defun c-c++/post-init-eldoc ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-eldoc)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-eldoc))

(defun c-c++/post-init-flycheck ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-flycheck)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-flycheck))

(defun c-c++/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t))

(defun c-c++/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))


(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :defer t
    :init
    (progn
      (when c-c++-enable-google-style
        (add-hook 'c-mode-common-hook 'google-set-c-style))
      (when c-c++-enable-google-newline
        (add-hook 'c-mode-common-hook 'google-make-newline-indent)))))

(defun c-c++/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(C . t))))

(defun c-c++/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (progn
      (when c-c++-lsp-cquery-cache-directory
        ;; Ignore lsp cache dir, in case user has opted for cache within project
        ;; source tree
        (add-to-list 'projectile-globally-ignored-directories
                     c-c++-lsp-cquery-cache-directory))
      (when c-c++-adopt-subprojects
        (setq projectile-project-root-files-top-down-recurring
              (append '("compile_commands.json"
                        ".cquery"
                        ".ccls")
                      projectile-project-root-files-top-down-recurring))))))

(defun c-c++/post-init-realgud()
  (dolist (mode c-c++-modes)
    (spacemacs/add-realgud-debugger mode "gdb")))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r." 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      (unless (boundp 'ycmd-global-config)
        (setq-default ycmd-global-config
                      (concat (configuration-layer/get-layer-path 'ycmd)
                              "global_conf.py")))
      (setq-default ycmd-parse-conditions '(save mode-enabled)))))
