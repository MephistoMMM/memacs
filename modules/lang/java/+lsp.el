;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(use-package! lsp-java
  :after lsp-clients
  :init
  (add-hook 'java-mode-local-vars-hook #'lsp!)
  (setq lsp-java-server-install-dir (concat doom-user-dot-local-dir "share/eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (concat doom-etc-dir "java-workspace"))
  :config
  ;; TODO keybinds
  ;; TODO treemacs integration (?)
  )
