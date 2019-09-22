;;; packages.el --- C-C++ layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (boundp 'c-c++-backend)
         (member c-c++-backend '(lsp-cquery lsp-ccls)))
  (configuration-layer/declare-layers '(lsp dap)))
