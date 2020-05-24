;;; config/lib/config.el -*- lexical-binding: t; -*-

(use-package! yamlmod-wrapper
  :defer t
  :commands (yamlmod-load yamlmod-read-file yamlmod-ypath-search)
  :load-path "~/.emacs.d/.local/dymodules/yamlmod")

(load! "+lib-memacs")

(when (featurep! +spacemacs)
  (load! "+lib-spacemacs"))
