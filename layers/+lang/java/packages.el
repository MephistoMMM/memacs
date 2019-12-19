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
        dap-mode
        flycheck
        ggtags
        counsel-gtags
        (java-mode :location built-in)
        maven-test-mode
        mvn
        (lsp-java :requires lsp-mode)
        ))

(defun java/post-init-company ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-company))

(defun java/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'java-mode)
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-lsp-dap))

(defun java/post-init-flycheck ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-flycheck))

(defun java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun java/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'java-mode))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (progn
      (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-backend)
      (add-hook 'java-mode-hook (lambda ()
                                  (setq c-basic-offset 4
                                        tab-width 4)))
      (put 'java-backend 'safe-local-variable 'symbolp)))
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(java . t)))
  )

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

(defun java/init-lsp-java ()
  (use-package lsp-java
    :defer t
    :config
    (progn
      ;; lombok
      (when (file-exists-p java-lombok-jar-path)
        (add-to-list 'lsp-java-vmargs (concat "-javaagent:" java-lombok-jar-path)))
      ;; key bindings
      (dolist (prefix '(("c" . "compile/create")
                        ("g" . "goto")
                        ("r" . "refactor")
                        ("ra" . "add/assign")
                        ("rc" . "create/convert")
                        ("rg" . "generate")
                        ("re" . "extract")
                        ("p" . "project")
                        ("q" . "lsp")
                        ("t" . "test")
                        ("x" . "execute")))
        (spacemacs/declare-prefix-for-mode 'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "pu"  'lsp-java-update-project-configuration

        ;; refactoring
        "ro" 'lsp-java-organize-imports
        "rcp" 'lsp-java-create-parameter
        "rcf" 'lsp-java-create-field
        "rci" 'lsp-java-conver-to-static-import
        "rec" 'lsp-java-extract-to-constant
        "rel" 'lsp-java-extract-to-local-variable
        "rem" 'lsp-java-extract-method

        ;; assign/add
        "rai" 'lsp-java-add-import
        "ram" 'lsp-java-add-unimplemented-methods
        "rat" 'lsp-java-add-throws
        "raa" 'lsp-java-assign-all
        "raf" 'lsp-java-assign-to-field

        ;; generate
        "rgt" 'lsp-java-generate-to-string
        "rge" 'lsp-java-generate-equals-and-hash-code
        "rgo" 'lsp-java-generate-overrides
        "rgg" 'lsp-java-generate-getters-and-setters

        ;; create/compile
        "cc"  'lsp-java-build-project
        "cp"  'lsp-java-spring-initializr

        "an"  'lsp-java-actionable-notifications
        ;; dap-mode
        ;; debug
        "ddj" 'dap-java-debug
        "dtt" 'dap-java-debug-test-method
        "dtc" 'dap-java-debug-test-class
        ;; run
        "tt" 'dap-java-run-test-method
        "tc" 'dap-java-run-test-class)

      (setq lsp-highlight-symbol-at-point nil
            lsp-ui-sideline-update-mode 'point
            lsp-eldoc-render-all nil
            lsp-inhibit-message t
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
