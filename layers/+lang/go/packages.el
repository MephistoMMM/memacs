;;; packages.el --- Go Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq go-packages
      '(
        (company-go :requires company)
        flycheck
        exec-path-from-shell
        go-eldoc
        go-mode
        go-guru
        go-rename
        godoctor
        go-tag
        popwin
        ))


(defun go/post-init-popwin ()
  (push (cons go-test-buffer-name '(:dedicated t :position bottom :stick t :noselect t :height 0.4))
        popwin:special-display-config))

(defun go/init-company-go ()
  (use-package company-go
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-go
      :modes go-mode
      :variables company-go-show-annotation t)))

(defun go/post-init-flycheck ()
  (spacemacs/enable-flycheck 'go-mode))

(defun go/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-config
    (dolist (var '("GOPATH" "GOROOT" "GO15VENDOREXPERIMENT") exec-path-from-shell-variables)
      (unless (or (member var exec-path-from-shell-variables) (getenv var))
        (push var exec-path-from-shell-variables)))))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :init
    (progn
      (add-hook 'go-mode-hook 'spacemacs//go-set-tab-width)
      ;; Change flycheck-disabled-checkers
      (with-eval-after-load 'flycheck
        (add-hook 'flycheck-mode-hook (lambda ()
                                        (add-to-list 'flycheck-disabled-checkers 'go-vet)
                                        (add-to-list 'flycheck-disabled-checkers 'go-gofmt)
                                        (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
                                        ))))
    :config (add-hook 'before-save-hook 'gofmt-before-save)))

(defun go/init-go-eldoc ()
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun go/init-go-guru ()
  (use-package go-guru
    :defer t))

(defun go/init-go-rename ()
  (use-package go-rename
    :defer t))

(defun go/init-godoctor ()
  (use-package godoctor
    :defer t))

(defun go/init-go-tag ()
  (use-package go-tag
    :defer t))
