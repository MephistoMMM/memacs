;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

(package! highlight-quoted :pin "2410347815")

;; Tools
(package! macrostep :pin "424e3734a1")
(package! overseer :pin "02d49f582e")
(package! elisp-def :pin "368b04da68")
(package! elisp-demos :pin "57dd4ae3e4")
(when (featurep! :checkers syntax)
  (package! flycheck-cask :pin "3457ae553c"))

;; Libraries
(package! buttercup :pin "a91f282025")
