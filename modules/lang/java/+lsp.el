;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(use-package! lsp-java
  :after lsp-clients
  :preface
  (setq lsp-java-server-install-dir (concat doom-user-dot-local-dir "share/eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (concat doom-etc-dir "java-workspace")
        lsp-jt-root (concat doom-user-dot-local-dir "share/eclipse.jdt.ls/server/java-test/server/"))
  (add-hook! java-mode-local-vars #'lsp!)
  (map! :when (featurep! :tools debugger +lsp)
          :after cc-mode ; where `java-mode' is defined
          :map java-mode-map
          :localleader
          (:prefix ("t" . "Test")
          :desc "Run test class or method"   "t" #'+java/run-test
          :desc "Run all tests in class"     "a" #'dap-java-run-test-class
          :desc "Debug test class or method" "d" #'+java/debug-test
          :desc "Debug all tests in class"   "D" #'dap-java-debug-test-class))
  :config
  (when (featurep! :tools debugger +lsp)
    (setq dap-java-test-runner
          (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))))
