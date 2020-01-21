;; -*- no-byte-compile: t; -*-
;;; config/lib/packages.el
;;
;; My Dynamic Modules Directory: ~/.emacs.d/.local/dymodules

(use-package! yamlmod-wrapper
  :defer t
  :commands (yamlmod-load yamlmod-read-file yamlmod-ypath-search)
  :load-path "~/.emacs.d/.local/dymodules/yamlmod")
