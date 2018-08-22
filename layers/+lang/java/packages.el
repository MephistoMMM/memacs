;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq java-packages
      '(
        company
        eldoc
        flycheck
        (java-mode :location built-in)
        maven-test-mode
        meghanada
        mvn
        (lsp-java :requires lsp-mode lsp-ui company-lsp)
        org
        ))

(defun java/post-init-company ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-company))

(defun java/post-init-eldoc ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-eldoc))

(defun java/post-init-flycheck ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-flycheck))

(defun java/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(java . t))))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (progn
      (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-backend)
      (put 'java-backend 'safe-local-variable 'symbolp)
      (spacemacs//java-define-command-prefixes))))

(defun java/init-maven-test-mode ()
  (use-package maven-test-mode
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (add-hook 'java-mode-hook 'maven-test-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "m" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'java-mode "mt" "tests"))
    :config
    (progn
      (spacemacs|hide-lighter maven-test-mode)
      (spacemacs/set-leader-keys-for-minor-mode 'maven-test-mode
        "mga"  'maven-test-toggle-between-test-and-class
        "mgA"  'maven-test-toggle-between-test-and-class-other-window
        "mta"   'maven-test-all
        "mtC-a" 'maven-test-clean-test-all
        "mtb"   'maven-test-file
        "mti"   'maven-test-install
        "mtt"   'maven-test-method))))

(defun java/init-meghanada ()
  (use-package meghanada
    :defer t
    :init
    (progn
      (setq meghanada-server-install-dir (concat spacemacs-cache-directory
                                                 "meghanada/")
            company-meghanada-prefix-length 1
            ;; let spacemacs handle company and flycheck itself
            meghanada-use-company nil
            meghanada-use-flycheck nil))
    :config
    (progn
      ;; key bindings
      (dolist (prefix '(("c" . "compile")
                        ("D" . "daemon")
                        ("g" . "goto")
                        ("r" . "refactor")
                        ("t" . "test")
                        ("x" . "execute")))
        (spacemacs/declare-prefix-for-mode
         'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "cb" 'meghanada-compile-file
        "cc" 'meghanada-compile-project

        "Dc" 'meghanada-client-direct-connect
        "Dd" 'meghanada-client-disconnect
        "Di" 'meghanada-install-server
        "Dk" 'meghanada-server-kill
        "Dl" 'meghanada-clear-cache
        "Dp" 'meghanada-ping
        "Dr" 'meghanada-restart
        "Ds" 'meghanada-client-connect
        "Du" 'meghanada-update-server
        "Dv" 'meghanada-version

        "gb" 'meghanada-back-jump

        "=" 'meghanada-code-beautify
        "ri" 'meghanada-optimize-import
        "rI" 'meghanada-import-all

        "ta" 'meghanada--run-junit
        "tc" 'meghanada-run-junit-class
        "tl" 'meghanada-run-junit-recent
        "tt" 'meghanada-run-junit-test-case

        ;; meghanada-switch-testcase
        ;; meghanada-local-variable

        "x:" 'meghanada-run-task))))

(defun java/init-lsp-java ()
  (use-package lsp-java
    :defer t
    :config
    (progn
      ;; key bindings
      (dolist (prefix '(("c" . "compile")
                        ("g" . "goto")
                        ("r" . "refactor")
                        ("q" . "lsp")))
        (spacemacs/declare-prefix-for-mode 'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "gg"  'xref-find-definitions
        "gr"  'xref-find-references
        "gR"  'lsp-ui-peek-find-references
        "ga"  'xref-find-apropos
        "gA"  'lsp-ui-peek-find-workspace-symbol
        "gd"  'lsp-goto-type-definition
        "hh"  'lsp-describe-thing-at-point
        "el"  'lsp-ui-flycheck-list
        "pu"  'lsp-java-update-user-settings
        "ea"  'lsp-execute-code-action
        "qr"  'lsp-restart-workspace
        "roi" 'lsp-java-organize-imports
        "rr" 'lsp-rename
        "rai" 'lsp-java-add-import
        "ram" 'lsp-java-add-unimplemented-methods
        "rcp" 'lsp-java-create-parameter
        "rcf" 'lsp-java-create-field
        "rec" 'lsp-java-extract-to-constant
        "rel" 'lsp-java-extract-to-local-variable
        "rem" 'lsp-java-extract-method
        "cc"  'lsp-java-build-project
        "an"  'lsp-java-actionable-notifications
        "="   'lsp-format-buffer)

      (setq lsp-highlight-symbol-at-point nil
            lsp-ui-sideline-update-mode 'point
            lsp-eldoc-render-all nil
            lsp-java-completion-guess-arguments t))))

(defun java/init-mvn ()
  (use-package mvn
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "m" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "mcc" 'mvn-compile
        "mcC" 'mvn-clean
        "mcr" 'spacemacs/mvn-clean-compile))))
