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
        company
        (company-c-headers :requires company)
        counsel-gtags
        ggtags

        ;; lsp
        (cquery :requires lsp-mode company-lsp)
        projectile
        ))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (when c-c++-default-mode-for-headers
        (add-to-list 'auto-mode-alist
                     `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
      (when c-c++-enable-auto-newline
        (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline)))
    :config (require 'compile)))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if (or c-c++-enable-clang-support (spacemacs//c-c++-lsp-enabled))
    :init
    (progn
      (when c-c++-enable-clang-format-on-save
        (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
      )))

(defun c-c++/post-init-company ()
  (when c-c++-enable-clang-support
    (if (spacemacs//c-c++-lsp-enabled)
      (display-warning :error "`c-c++-enable-clang-support' ignored when using lsp backend")
      (progn
        (spacemacs|add-company-backends :backends company-clang :modes c-mode-common)
        (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
        (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)
        ()))))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

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

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)))

(defun c-c++/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))


(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :defer
    :if (or 'c-c++-enable-google-style 'c-c++-enable-google-newline)
    :init (progn
            (when c-c++-enable-google-style (add-hook 'c-mode-common-hook 'google-set-c-style))
            (when c-c++-enable-google-newline (add-hook 'c-mode-common-hook 'google-make-newline-indent)))))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/load-stickyfunc-enhance c-c++-mode-hooks))

;; BEGIN LSP BACKEND PACKAGES
;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun c-c++/init-cquery ()
  (use-package cquery
    :if (eq c-c++-backend 'lsp-cquery)
    :config
    (spacemacs//c-c++-lsp-config)
    :hook ((c-mode c++-mode) .
            (lambda () (cl-pushnew #'company-lsp company-backends) (require 'cquery) (remhash 'clangd lsp-clients) (lsp)))))

;;Intentionally adding both cquery and ccls cache dirs to ignore list, to facilitate switching between
;;two without multiple caches polluting projectile find file results
(defun c-c++/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (progn
      (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
      (when c-c++-lsp-cache-dir
        (add-to-list 'projectile-globally-ignored-directories c-c++-lsp-cache-dir))
      (when c-c++-adopt-subprojects
        (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".cquery"
                    ".ccls")
            projectile-project-root-files-top-down-recurring))))))

;; END LSP BACKEND PACKAGES
