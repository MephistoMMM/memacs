;;; config.el --- Python Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers python-mode)

(defvar python-backend 'lsp
  "The backend to use for IDE features.
Possible values are `anaconda' and `lsp'.")

(defvar python-lsp-server 'mspyls
  "Language server to use for lsp backend. Possible values are `pyls'
and `mspyls'")

(defvar python-lsp-git-root nil
  "If non-nil, use a development version of the language server in this folder")

(defvar python-formatter 'yapf
  "The formatter to use. Possible values are `yapf',
  `black' and `lsp'. If left empty a default will be selected based on the backend.")

(defvar python-format-on-save nil
  "If non-nil, automatically format code with formatter selected
  via `python-formatter' on save.")

(defvar python-test-runner 'pytest
  "Test runner to use. Possible values are `nose' or `pytest'.")

(defvar python-save-before-test t
  "If non nil, current buffer will be save before call a test function")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar python-tab-width 4
  "Tab width value for python buffers")

(defvar python-auto-set-local-pyvenv-virtualenv 'on-visit
  "Automatically set pyvenv virtualenv from \".venv\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar python-sort-imports-on-save nil
  "If non-nil, automatically sort imports on save.")
(defvar spacemacs--python-pyvenv-modes nil
  "List of major modes where to add pyvenv support.")

;; inferior-python-mode needs these variables to be defined.  The python
;; package declares them but does not initialize them.
(defvar python-shell--interpreter nil)
(defvar python-shell--interpreter-args nil)
