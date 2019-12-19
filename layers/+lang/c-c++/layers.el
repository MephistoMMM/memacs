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

(when (fboundp 'spacemacs//c-c++-backend)
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (configuration-layer/declare-layer-dependencies '(lsp dap)))
    (`lsp-ccls (configuration-layer/declare-layer-dependencies '(lsp dap)))
    (`lsp-cquery (configuration-layer/declare-layer-dependencies '(lsp dap)))
    (`ycmd (configuration-layer/declare-layer-dependencies '(ycmd)))))
