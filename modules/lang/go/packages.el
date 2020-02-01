;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc :pin "cbbd2ea1e9")
(package! go-guru :pin "53c76cddf5")
(package! go-mode :pin "53c76cddf5")
(package! gorepl-mode :pin "6a73bf352e")
(package! go-tag :pin "59b243f2fa")
(package! go-gen-test :pin "44c202ac97")

(when (featurep! :completion company)
  (package! company-go :pin "939b4a677f"))

(when (featurep! :checkers syntax)
  (package! flycheck-golangci-lint :pin "8e446c6831"))
