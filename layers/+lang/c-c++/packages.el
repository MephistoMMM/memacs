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
    srefactor
    stickyfunc-enhance

    ;; normal
    clang-format
    company
    (company-c-headers :requires company)
    counsel-gtags
    ggtags

    ;; lsp
    (cquery :requires lsp-mode company-lsp)
    ))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist `("\\.h\\'" . ,c-c++-default-mode-for-headers))
      (when c-c++-enable-auto-newline
        (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline)))
    :config (require 'compile)))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if c-c++-enable-clang-support
    :init
    (progn
      (when c-c++-enable-clang-format-on-save
        (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
      )))

(defun c-c++/post-init-company ()
  (when c-c++-enable-clang-support
    (spacemacs|add-company-backends :backends company-clang
      :modes c-mode-common)
    (when c-c++-enable-c++11
      (setq company-clang-arguments '("-std=c++11")))
    (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode)))

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

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r" 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/load-stickyfunc-enhance c-c++-mode-hooks))

;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun c-c++/init-cquery ()
  (use-package cquery
    :commands lsp-cquery-enable
    :defer t
    :init
    (progn
      (setq cquery-executable "/usr/local/bin/cquery")
      ;; Customize `lsp-project-whitelist' `lsp-project-blacklist' to disable auto initialization.
      (add-hook 'c-mode-common-hook #'memacs//c-c++-cquery-enable)

      (with-eval-after-load 'projectile
        (setq projectile-project-root-files-top-down-recurring
              (append '("compile_commands.json"
                        ".cquery")
                      projectile-project-root-files-top-down-recurring))
        (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index"))

      ;; add company-lsp
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))
    ))
