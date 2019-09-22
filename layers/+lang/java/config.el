;;; packages.el --- Java configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers java-mode)

(defvar java-backend 'lsp
  "The backend to use for IDE features. Possible values are `lsp' and `meghanada'.")

(setq lsp-java-server-install-dir
      (expand-file-name "~/.local/share/eclipse.jdt.ls/server/"))
;; TODO: make this value different from each projection.
(setq lsp-java--workspace-folders '("~/Workspace/java/group4"))
