;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "4c08ef468678bbf3b3c9e750f6e694eea1aa8423")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "156f65cfbf690ed84e0e84f90277d665d873ff24"))
(when (featurep! +tabnine)
  (package! company-tabnine :pin "98e9e8b38b6ca289fbe265b0a7b62c7fe38ed0e2"))
