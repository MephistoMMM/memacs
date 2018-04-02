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
        counsel-gtags
        (java-mode :location built-in)
        maven-test-mode
        mvn
        ))

(defun java/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'java-mode))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t))

(defun java/init-maven-test-mode ()
  (use-package maven-test-mode
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (add-hook 'java-mode-hook 'maven-test-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "mm" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmg" "goto")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmt" "tests"))
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

(defun java/init-mvn ()
  (use-package mvn
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "mm" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "mcc" 'mvn-compile
        "mcC" 'mvn-clean
        "mcr" 'spacemacs/mvn-clean-compile))))
