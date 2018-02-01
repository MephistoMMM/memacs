;;; packages.el --- python layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; python-mode

(spacemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
(spacemacs/declare-prefix-for-mode 'python-mode "md" "debug")
(spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
(spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
(spacemacs/declare-prefix-for-mode 'python-mode "ms" "REPL")
(spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
(spacemacs/declare-prefix-for-mode 'python-mode "mv" "pyenv")
(spacemacs/declare-prefix-for-mode 'python-mode "mV" "pyvenv")

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "'"  'spacemacs/python-start-or-switch-repl
  "cc" 'spacemacs/python-execute-file
  "cC" 'spacemacs/python-execute-file-focus
  "db" 'spacemacs/python-toggle-breakpoint
  "ri" 'spacemacs/python-remove-unused-imports
  "sB" 'spacemacs/python-shell-send-buffer-switch
  "sb" 'python-shell-send-buffer
  "sF" 'spacemacs/python-shell-send-defun-switch
  "sf" 'python-shell-send-defun
  "si" 'spacemacs/python-start-or-switch-repl
  "sR" 'spacemacs/python-shell-send-region-switch
  "sr" 'python-shell-send-region)

;;;; importmagic
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "rf" 'importmagic-fix-symbol-at-point)

;;;; live py mode
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "l" 'live-py-mode)

;;;; nose
(spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "tA" 'spacemacs/python-test-pdb-all
  "ta" 'spacemacs/python-test-all
  "tB" 'spacemacs/python-test-pdb-module
  "tb" 'spacemacs/python-test-module
  "tT" 'spacemacs/python-test-pdb-one
  "tt" 'spacemacs/python-test-one
  "tM" 'spacemacs/python-test-pdb-module
  "tm" 'spacemacs/python-test-module
  "tS" 'spacemacs/python-test-pdb-suite
  "ts" 'spacemacs/python-test-suite)

;;;; pippel
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "P" 'pippel-list-packages)

;;;; py isort
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "rI" 'py-isort-buffer)

;;;; pyenv
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "vu" 'pyenv-mode-unset
  "vs" 'pyenv-mode-set)

;;;; pyvenv
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "Va" 'pyvenv-activate
  "Vd" 'pyvenv-deactivate
  "Vw" 'pyvenv-workon)

;;;; pylookup
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "hH" 'pylookup-lookup)

;;;; xcscope
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "gi" 'cscope/run-pycscope)

;;;; lsp
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "=" 'lsp-format-buffer)

;;;; xref
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  (kbd "gr") #'xref-find-references)
