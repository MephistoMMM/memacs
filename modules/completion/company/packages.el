;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "97cfbc3967c195fb4ccb171735b9b1dea97e681a")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "be37a9a30dc112ab172af21af694e2cb04a74f85"))
(when (featurep! +tabnine)
  (package! company-tabnine :pin "e986a4ad0d0e0174b08f1fb94c4f804a98a344e4"))
