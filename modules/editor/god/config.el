;;; editor/god/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :hook (doom-after-init-modules . god-mode-all)
  :config
  (add-hook 'post-command-hook #'+god--configure-cursor-and-modeline-h)
  (add-hook 'overwrite-mode-hook #'+god--toggle-on-overwrite-h))
