;;; mp-base/packages.el --- Defined packages and some base configs for mp-base.

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp mp-base packages

;;; Commentary:

;;

;;; Code:
(configuration-layer/declare-layers '(
                                      ;; helm
                                      spacemacs-completion
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
