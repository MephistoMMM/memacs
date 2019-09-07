;;; memacs/packages.el --- Defined packages and some base configs for memacs.

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp memacs packages

;;; Commentary:

;;

;;; Code:
(configuration-layer/declare-layers
 '(
   (ivy :variables ivy-enable-advanced-buffer-information t)
   spacemacs-defaults
   spacemacs-project
   spacemacs-layouts
   spacemacs-editing
   spacemacs-editing-visual
   spacemacs-evil
   (evil-snipe :variables
               evil-snipe-enable-alternate-f-and-t-behaviors t
               evil-snipe-scope 'whole-visible
               evil-snipe-repeat-keys nil
               )
   spacemacs-modeline
   spacemacs-misc
   spacemacs-navigation
   awesome-tab
   spacemacs-org
   spacemacs-visual
   multiple-cursors

   (auto-completion :variables
                    auto-completion-enable-sort-by-usage nil
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-idle-delay 0.3
                    auto-completion-private-snippets-directory "~/.emacs.d/snippets/")
   (syntax-checking :variables syntax-checking-enable-by-default nil)
   lsp
   git
   (org :variables
        org-startup-indented t
        org-enable-github-support t)
   (templates :variables auto-insert-query nil)
   ))

;;; packages.el ends here
