;;; memacs/packages.el --- Defined packages and some base configs for memacs.

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp memacs packages

;;; Commentary:

;;

;;; Code:
(configuration-layer/declare-layers '(
                                      ivy
                                      spacemacs-base
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
                                      ;; spacemacs-purpose
                                      spacemacs-visual
                                      ))
;;; packages.el ends here
