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

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)

(defvar c-c++-backend nil
  "The backend to use for IDE features.
Possible values are `lsp-ccls', `lsp-cquery', `lsp-clangd', `rtags' and `ycmd'.")


;; lsp

(defconst c-c++-lsp-backends '(lsp-cquery)
  "Language Server Protocol (LSP) backends supported by the `c-c++' layer.")

(defvar c-c++-backend 'lsp-cquery
  "If `lsp-cquery' or `lsp-ccls' then selects language server protocol backend (cquery or ccls).
  If `rtags' then enables rtags support")

(defvar c-c++-lsp-cquery-cache-directory nil
  "Cache directory for lsp-cquery backends.
Can be nil, an absolute path or a relative path.
If it is nil then the cache directory is in `spacemacs-cache-directory'.
If it is a relative path then it is relative to the project root.")


;; clang

(defvar c-c++-enable-clang-format-on-save nil
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")


;; style

(defvar c++-enable-organize-includes-on-save nil
  "If non-nil then automatically organize the includes on save C++ buffer.")

(defvar c-c++-enable-auto-newline nil
  "If non-nil press `;' will add newline after semicolon.")

(defvar c-c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-google-newline nil
  "If non-nil `google-make-newline-indent' will be added as as
  `c-mode-common-hook'.")


;; misc

(defvar c-c++-default-mode-for-headers (when (not (functionp 'c-or-c++-mode)) 'c-mode)
  "Default mode to open header files. Can be `c-mode' or `c++-mode', or `c-or-c++-mode' for Emacs > 26+.")

(defvar c-c++-adopt-subprojects nil
  "When non-nil, projectile will remember project root when visiting files in subprojects")


;; internal

(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")
