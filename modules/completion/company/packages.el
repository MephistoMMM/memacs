;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "ed46a616ab9906fd43a630479b6a6c3f79e606f0")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "b6f53e26adf948aca55c3ff6c22c21a6a6614253"))
(when (modulep! +tabnine)
  (package! company-tabnine :pin "98e9e8b38b6ca289fbe265b0a7b62c7fe38ed0e2"))
