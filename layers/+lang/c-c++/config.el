;;; config.el --- C/C++ Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")

(defvar c-c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-google-newline nil
  "If non-nil `google-make-newline-indent' will be added as as
  `c-mode-common-hook'.")

(spacemacs|define-jump-handlers c++-mode xref-find-definitions)
(spacemacs|define-jump-handlers c-mode   xref-find-definitions)

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

;; lsp ccls
;; See https://github.com/MaskRay/ccls/wiki/Initialization-options
(defvar ccls-extra-init-params '(:completion (:detailedLabel t)
                                 :diagnostics (:frequencyMs 5000)
                                 :index (:reparseForDependency 1)))
