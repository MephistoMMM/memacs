;;; memacs/packages.el --- Defined packages and some base configs for memacs.

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp memacs packages

;;; Commentary:

;;

;;; Code:
(configuration-layer/declare-layers
 '(
   ivy
   spacemacs-defaults
   spacemacs-project
   (ibuffer :variables ibuffer-group-buffers-by 'projects)
   (dired :variables ranger-show-preview t)
   spacemacs-layouts
   spacemacs-editing
   spacemacs-editing-visual
   spacemacs-evil
   (evil-snipe :variables
               evil-snipe-enable-alternate-f-and-t-behaviors t
               evil-snipe-scope 'whole-visible
               evil-snipe-repeat-keys nil
               )
   spacemacs-misc
   spacemacs-navigation
   spacemacs-org
   spacemacs-visual


   (auto-completion :variables
                    auto-completion-enable-sort-by-usage nil
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-idle-delay 0.6
                    auto-completion-private-snippets-directory "~/.emacs.d/snippets/")
   (syntax-checking :variables syntax-checking-enable-by-default nil)
   lsp
   git
   (org :variables
        org-startup-indented t
        org-enable-github-support t)
   ))

;;; packages.el ends here
